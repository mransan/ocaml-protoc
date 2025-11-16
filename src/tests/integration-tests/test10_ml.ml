module T = Test10

let decode_pb_ref_data () =
  T.make_m10 ~m10_f1:(Test09.make_m09 ~int32_f1:1l ()) ()

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test10.c2ml.data" T.decode_pb_m10 T.pp_m10
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test10.ml2c.data" T.encode_pb_m10 (decode_pb_ref_data ())
