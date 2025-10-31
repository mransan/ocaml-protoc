module T = Test02

let decode_ref_data () =
  T.make_all_basics_types ~o01:1.0 ~o02:2.0 ~o03:(-123l) ~o04:456L ~o05:123l
    ~o06:456L ~o07:(-123l) ~o08:(-456L) ~o09:0xFFFFFFFFl
    ~o10:0xFFFFFFFFFFFFFFFFL ~o11:0xFFFFFFFFl ~o12:0xFFFFFFFFFFFFFFFFL ~o13:true
    ~o14:"Iam a test string"
    ~o15:(Bytes.of_string "Iam a test byte")
    ()

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test02.c2ml.data" T.decode_pb_all_basics_types
      T.pp_all_basics_types (decode_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test02.ml2c.data" T.encode_pb_all_basics_types
      (decode_ref_data ())
