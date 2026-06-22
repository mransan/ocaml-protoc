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

let () =
  test_empty_roundtrip ();
  test_num_roundtrip ()
