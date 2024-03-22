module T = Test07

let decode_ref_data () =
  {
    T.value = 1l;
    T.left =
      {
        t =
          Some
            (T.Node
               {
                 T.value = 2l;
                 T.left = { t = Some (T.Empty 0l) };
                 T.right = { t = Some (T.Empty 0l) };
               });
      };
    T.right =
      {
        t =
          Some
            (T.Node
               {
                 T.value = 3l;
                 T.left = { t = Some (T.Empty 0l) };
                 T.right = { t = Some (T.Empty 0l) };
               });
      };
  }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test07.c2ml.data" T.decode_pb_node T.pp_node
      (decode_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test07.ml2c.data" T.encode_pb_node (decode_ref_data ())
