module T = Test14

let decode_pb_ref_data () =
  T.
    {
      da = { aa = [ 1l; 2l; 3l; 4l ] };
      db = { sub = Some (Ba { aa = [ 1l; 2l; 3l; 4l ] }) };
      dc =
        {
          sub = Some (Ca { aa = [ 1l; 2l; 3l; 4l ] });
          cc = Some { aa = [ 1l; 2l; 3l; 4l ] };
        };
    }

let mode = Test_util.parse_args ()

let () =
  match mode with
  | Test_util.Decode ->
    Test_util.decode "test14.c2ml.data" T.decode_pb_d T.pp_d
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test14.ml2c.data" T.encode_pb_d (decode_pb_ref_data ())
