(* see:
   https://developers.google.com/protocol-buffers/docs/encoding#varints
*)

module D = Pbrt.Decoder
module E = Pbrt.Encoder

let decvarint (s : string) : int64 =
  let dec = D.of_string s in
  D.int64_as_varint dec

let encvarint (i : int64) : string =
  let enc = E.create () in
  E.int64_as_varint i enc;
  E.to_string enc

let str_to_l s =
  let l = ref [] in
  String.iter (fun x -> l := x :: !l) s;
  List.rev !l

let str_to_il s = str_to_l s |> List.map Char.code

let () =
  let s = encvarint 12L in
  assert (str_to_il s = [ 12 ]);
  assert (decvarint s = 12L)

let () =
  let s = encvarint 0L in
  assert (str_to_il s = [ 0 ]);
  assert (decvarint s = 0L)

let () =
  let s = encvarint 127L in
  assert (str_to_il s = [ 127 ]);
  assert (decvarint s = 127L)

let () =
  let s = encvarint 128L in
  assert (str_to_il s = [ 128; 1 ]);
  assert (decvarint s = 128L)

let () =
  let s = encvarint 300L in
  assert (str_to_il s = [ 0b1010_1100; 0b0000_0010 ]);
  assert (decvarint s = 300L)

let () =
  let s = encvarint 150L in
  assert (str_to_il s = [ 0x96; 1 ]);
  assert (decvarint s = 150L)

let () =
  let s = encvarint 178282982111149L in
  assert (str_to_il s = [ 173; 239; 197; 238; 219; 196; 40 ]);
  assert (decvarint s = 178282982111149L)

(* zigzag round-trip tests — regression for shift_right vs shift_right_logical bug *)
let roundtrip_zigzag v =
  let enc = E.create () in
  E.int64_as_zigzag v enc;
  let dec = D.of_bytes (E.to_bytes enc) in
  let got = D.int64_as_zigzag dec in
  assert (Int64.equal v got)

let () =
  roundtrip_zigzag 0L;
  roundtrip_zigzag 1L;
  roundtrip_zigzag (-1L);
  roundtrip_zigzag (-2L);
  roundtrip_zigzag 0x4000000000000000L;
  roundtrip_zigzag Int64.max_int;
  (* was decoded as -1L before fix *)
  roundtrip_zigzag Int64.min_int
