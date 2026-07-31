module D = Pbrt.Decoder

(* 21-byte protoc-validated EmptyEvolved payload:
 * varint 150, len-2 "hi", fixed32 7, fixed64 9 *)
let payload =
  "\x08\x96\x01\x12\x02\x68\x69\x1d\x07\x00\x00\x00\x21\x09\x00\x00\x00\x00\x00\x00\x00"

(* same payload truncated to its first 20 bytes: the final fixed64 is
 * one byte short *)
let payload_truncated =
  "\x08\x96\x01\x12\x02\x68\x69\x1d\x07\x00\x00\x00\x21\x09\x00\x00\x00\x00\x00\x00"

let check name ok =
  if ok then
    ()
  else (
    print_endline ("FAIL: " ^ name);
    exit 1
  )

let raises_incomplete f =
  try
    f ();
    false
  with D.Failure D.Incomplete -> true

(* zero-length empty message: succeeds, input exhausted *)
let () =
  let d = D.of_string "\x00" in
  D.empty_nested d;
  check "zero-length" (D.key d = None)

(* empty message carrying unknown fields from a newer schema:
 * succeeds and consumes the whole payload *)
let () =
  let d = D.of_string ("\x15" ^ payload) in
  D.empty_nested d;
  check "evolved payload skipped" (D.key d = None)

(* same, nested one level down as field 1 with wire type 2 *)
let () =
  let d = D.of_string ("\x0a\x15" ^ payload) in
  check "nested framing key" (D.key d = Some (1, Pbrt.Bytes));
  D.empty_nested d;
  check "nested framing exhausted" (D.key d = None)

(* truncated payload: declared length exceeds remaining input *)
let () =
  let d = D.of_string ("\x15" ^ payload_truncated) in
  check "truncated raises Incomplete"
    (raises_incomplete (fun () -> D.empty_nested d))

(* hostile 10-byte varint length 2^64-1 wraps to -1 through
 * Int64.to_int: caught by the negative-length guard *)
let () =
  let d = D.of_string "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01" in
  check "negative length raises Incomplete"
    (raises_incomplete (fun () -> D.empty_nested d))

(* overflow neighbor of the negative case: the 9-byte varint decodes
 * through Int64.to_int to exactly max_int, which stays POSITIVE, so an
 * addition-form bound check (offset + len > limit) would wrap negative
 * and silently pass; only the subtraction form catches it *)
let () =
  let d = D.of_string "\xff\xff\xff\xff\xff\xff\xff\xff\x3f" in
  check "max_int length raises Incomplete"
    (raises_incomplete (fun () -> D.empty_nested d))

(* skip-side twin: this 9-byte varint decodes through Int64.to_int to
 * exactly -10, so an unguarded skip_len would move the offset BACKWARDS
 * (silently returning unit here, an infinite loop in generated skip
 * loops); the negative guard must turn it into Incomplete *)
let () =
  let d = D.of_string "\xf6\xff\xff\xff\xff\xff\xff\xff\x7f" in
  check "negative skip length raises Incomplete"
    (raises_incomplete (fun () -> D.skip d Pbrt.Bytes))

let () = print_endline "empty_nested tests passed"
