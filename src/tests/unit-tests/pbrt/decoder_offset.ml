module D = Pbrt.Decoder
module E = Pbrt.Encoder

(* basic of_bytes: starts at 0 *)
let () =
  let buf = Bytes.create 10 in
  (* fill buf with some bytes *)
  Bytes.fill buf 0 10 'B';
  let d = D.of_bytes buf in
  assert (D.offset d = 0);
  (* skip some bytes to advance *)
  let _ = D.int_as_varint d in
  let pos = D.offset d in
  assert (pos > 0);
  ()

(* of_subbytes: offset is relative to the subview start *)
let () =
  let buf = Bytes.create 100 in
  Bytes.fill buf 0 100 'B';
  (* encode a known small varint at offset 50 *)
  let sub = Bytes.sub buf 50 10 in
  Bytes.set sub 0 (Char.chr 5);
  let d = D.of_subbytes buf 50 10 in
  assert (D.offset d = 0);
  let _ = D.int_as_varint d in
  (* after reading one byte, offset should be 1 *)
  assert (D.offset d = 1);
  ()

(* of_subbytes: same but with of_string and of_substring *)
let () =
  let s = String.make 100 '\x00' in
  let d = D.of_substring s 50 10 in
  assert (D.offset d = 0);
  ()

(* nested: offset is relative to the nested frame *)
let () =
  (* use E.nested to produce length-prefixed content, then decode it *)
  let enc = E.create () in
  E.nested
    (fun () enc ->
      E.int_as_varint 7 enc;
      E.int_as_varint 42 enc)
    () enc;
  let buf = E.to_bytes enc in
  let d = D.of_bytes buf in
  let nested_d = D.nested d in
  (* offset inside nested should be 0 *)
  assert (D.offset nested_d = 0);
  let _ = D.int_as_varint nested_d in
  (* after reading one varint byte, offset should be 1 *)
  assert (D.offset nested_d = 1);
  ()

let () = print_endline "offset tests passed"
