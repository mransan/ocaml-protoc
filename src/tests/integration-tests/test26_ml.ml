module T = Test26

let round32 v =
  let open Int32 in
  v |> bits_of_float |> float_of_bits

let decode_pb_ref_data () =
  T.
    {
      double_value = Some 1.23;
      float_value = Some (round32 1.23);
      int32_value = Some (-123l);
      uint32_value = Some 123l;
      int64_value = Some (-123L);
      uint64_value = Some 123L;
      bool_value = Some true;
      string_value = Some "abc";
      bytes_value = Some (Bytes.of_string "abc");
    }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test26.c2ml.data" T.decode_pb_test T.pp_test
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test26.ml2c.data" T.encode_pb_test (decode_pb_ref_data ())
