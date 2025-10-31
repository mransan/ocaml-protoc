module T = Test14

let decode_pb_ref_data () =
  T.make_d
    ~da:(T.make_a ~aa:[ 1l; 2l; 3l; 4l ] ())
    ~db:(Ba (T.make_a ~aa:[ 1l; 2l; 3l; 4l ] ()))
    ~dc:
      (T.make_c
         ~sub:(Ca (T.make_a ~aa:[ 1l; 2l; 3l; 4l ] ()))
         ~cc:(T.make_a ~aa:[ 1l; 2l; 3l; 4l ] ())
         ())
    ()

let mode = Test_util.parse_args ()

let () =
  match mode with
  | Test_util.Decode ->
    Test_util.decode "test14.c2ml.data" T.decode_pb_d T.pp_d
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test14.ml2c.data" T.encode_pb_d (decode_pb_ref_data ())
