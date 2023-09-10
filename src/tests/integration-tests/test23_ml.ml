module T = Test23

let decode_pb_ref_data () = { T.i = Int64.(shift_left 3L 56) }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test23.c2ml.data" T.decode_pb_int64_t T.pp_int64_t
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test23.ml2c.data" T.encode_pb_int64_t
      (decode_pb_ref_data ())
