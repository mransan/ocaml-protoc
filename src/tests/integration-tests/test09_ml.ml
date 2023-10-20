module T = Test09

let decode_pb_ref_data () = { T.int32_f1 = 1l }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test09.c2ml.data" T.decode_pb_m09 T.pp_m09
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test09.ml2c.data" T.encode_pb_m09 (decode_pb_ref_data ())
