module T = Test28

let test_data () : T.test =
  T.make_test ~enum1:Value_with_option
    ~unit_:(T.make_unit_ ~unit_field:1l ())
    ~one_of_is_keyword:(One_of_is_keyword_field 1l) ()

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test28.c2ml.data" T.decode_pb_test T.pp_test
      (test_data ())
  | Test_util.Encode ->
    Test_util.encode "test28.ml2c.data" T.encode_pb_test (test_data ())
