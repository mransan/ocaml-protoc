module T = Test25

let decode_pb_ref_data () = T.{ f1 = [ 1l; 2l; 3l; 4l ] }
let mode = Test_util.parse_args ()

let () =
  match mode with
  | Test_util.Decode ->
    Test_util.decode "test25.c2ml.data" T.decode_pb_m T.pp_m
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test25.ml2c.data" T.encode_pb_m (decode_pb_ref_data ())
