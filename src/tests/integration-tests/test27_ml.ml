module T = Test27

let test_data () =
  T.
    {
      enum1 = Value_with_option;
      unit_ = Some { unit_field = 1l };
      one_of_is_keyword = Some { message = Some (One_of_is_keyword_field 1l) };
    }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test27.c2ml.data" T.decode_pb_test T.pp_test
      (test_data ())
  | Test_util.Encode ->
    Test_util.encode "test27.ml2c.data" T.encode_pb_test (test_data ())
