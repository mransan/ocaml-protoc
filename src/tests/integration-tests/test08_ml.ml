module T = Test08

let decode_ref_data () =
  T.Node
    {
      T.value = 1l;
      T.left = T.Node { T.value = 2l; T.left = T.Empty 0l; right = T.Empty 0l };
      T.right = T.Node { T.value = 3l; T.left = T.Empty 0l; right = T.Empty 0l };
    }

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test08.c2ml.data" T.decode_pb_tree T.pp_tree
      (decode_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test08.ml2c.data" T.encode_pb_tree (decode_ref_data ())
