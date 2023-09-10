module T = Test12

let decode_pb_ref_data () = T.{ to_ = 1l; type_ = 1l }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test12.c2ml.data" T.decode_pb_m T.pp_m
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test12.ml2c.data" T.encode_pb_m (decode_pb_ref_data ())
