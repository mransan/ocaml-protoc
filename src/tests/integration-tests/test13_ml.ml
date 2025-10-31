module T = Test13

let decode_pb_ref_data () =
  T.make_t
    ~p2:(T.make_contains_empty ~empty:() ~sub:Sub_empty ())
    ~p1:(T.make_empty_or_int_list ~l:[ Empty; Int 1l; Empty; Int 2l ] ())
    ()

let mode = Test_util.parse_args ()

let () =
  match mode with
  | Test_util.Decode ->
    Test_util.decode "test13.c2ml.data" T.decode_pb_t T.pp_t
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test13.ml2c.data" T.encode_pb_t (decode_pb_ref_data ())
