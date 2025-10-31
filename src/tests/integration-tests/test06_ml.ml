module T = Test06

let decode_ref_data () = T.make_test_m ~teste_field:T.Test_e_value1 ()

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test06.c2ml.data" T.decode_pb_test_m T.pp_test_m
      (decode_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test06.ml2c.data" T.encode_pb_test_m (decode_ref_data ())
