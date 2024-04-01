module T = Test24

let decode_pb_ref_data () : T.a = T.{ x = Some (Value "value") }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test24.c2ml.data" T.decode_pb_a T.pp_a
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test24.ml2c.data" T.encode_pb_a (decode_pb_ref_data ())
