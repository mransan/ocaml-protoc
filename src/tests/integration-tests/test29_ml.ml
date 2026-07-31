module T = Test29

let test_empty_roundtrip () =
  (* Test that a oneof with an empty message round-trips correctly *)
  let v = T.Empty in
  let enc = Pbrt.Encoder.create () in
  T.encode_pb_outer v enc;
  let s = Pbrt.Encoder.to_string enc in
  let dec = Pbrt.Decoder.of_string s in
  let v' = T.decode_pb_outer dec in
  assert (v = v')

let test_num_roundtrip () =
  (* Test that a normal oneof variant still works *)
  let v = T.Num 42l in
  let enc = Pbrt.Encoder.create () in
  T.encode_pb_outer v enc;
  let s = Pbrt.Encoder.to_string enc in
  let dec = Pbrt.Decoder.of_string s in
  let v' = T.decode_pb_outer dec in
  assert (v = v')

(* #250: an "empty" message may carry fields written by a newer schema;
 * the generated decoder must skip them, not raise.
 * 21-byte protoc-validated payload: varint 150, len-2 "hi", fixed32 7,
 * fixed64 9 *)
let evolved_payload =
  "\x08\x96\x01\x12\x02\x68\x69\x1d\x07\x00\x00\x00\x21\x09\x00\x00\x00\x00\x00\x00\x00"

(* same payload truncated to its first 20 bytes: the final fixed64 is
 * one byte short *)
let evolved_payload_truncated =
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
  with Pbrt.Decoder.Failure Pbrt.Decoder.Incomplete -> true

let test_empty_decode_evolved () =
  (* generated decoder for the empty message skips unknown fields *)
  let dec = Pbrt.Decoder.of_string evolved_payload in
  let () = T.decode_pb_empty dec in
  check "empty decode evolved" (Pbrt.Decoder.key dec = None)

let test_empty_decode_truncated () =
  let dec = Pbrt.Decoder.of_string evolved_payload_truncated in
  check "empty decode truncated raises Incomplete"
    (raises_incomplete (fun () -> T.decode_pb_empty dec))

let test_outer_oneof_empty_evolved () =
  (* oneof arm [empty] is field 2 in test29.proto: tag byte
   * (2 lsl 3) lor 2 = 0x12, then length 0x15 = 21 *)
  let dec = Pbrt.Decoder.of_string ("\x12\x15" ^ evolved_payload) in
  check "outer oneof empty evolved" (T.decode_pb_outer dec = T.Empty)

let test_outer_oneof_empty_maxint_len () =
  (* oneof arm [empty] tagged 0x12, then a 9-byte varint length that
   * decodes to exactly max_int: an addition-form bound check would wrap
   * negative and silently accept this malformed message (pre-fix PoC
   * returned Empty); the subtraction-form guard must raise Incomplete *)
  let dec = Pbrt.Decoder.of_string "\x12\xff\xff\xff\xff\xff\xff\xff\xff\x3f" in
  check "outer oneof empty max_int length raises Incomplete"
    (raises_incomplete (fun () -> ignore (T.decode_pb_outer dec)))

let () =
  test_empty_roundtrip ();
  test_num_roundtrip ();
  test_empty_decode_evolved ();
  test_empty_decode_truncated ();
  test_outer_oneof_empty_evolved ();
  test_outer_oneof_empty_maxint_len ()
