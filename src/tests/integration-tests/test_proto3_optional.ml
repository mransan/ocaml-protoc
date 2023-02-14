open Test_proto3_optional_types
open Test_proto3_optional_pp
module Pb = Test_proto3_optional_pb

let foo1 : foo = { x = 1l; y = None; name = Some "hello" }

let () =
  let enc = Pbrt.Encoder.create () in
  Pb.encode_foo foo1 enc;
  let s = Pbrt.Encoder.to_string enc in
  let dec = Pbrt.Decoder.of_string s in
  let foo1' = Pb.decode_foo dec in
  assert (foo1 = foo1')
