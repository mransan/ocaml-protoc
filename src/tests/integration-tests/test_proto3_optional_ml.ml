open Test_proto3_optional

let foo1 : foo = { x = 1l; y = None; name = Some "hello" }

let () =
  let enc = Pbrt.Encoder.create () in
  encode_pb_foo foo1 enc;
  let s = Pbrt.Encoder.to_string enc in
  let dec = Pbrt.Decoder.of_string s in
  let foo1' = decode_pb_foo dec in
  assert (foo1 = foo1')
