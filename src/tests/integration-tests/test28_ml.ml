module T = Test28

let test_data () : T.person = T.make_person ~ofield33:false ()

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test28.c2ml.data" T.decode_pb_person T.pp_person
      (test_data ())
  | Test_util.Encode ->
    Test_util.encode "test28.ml2c.data" T.encode_pb_person (test_data ())
