module T = Test08

let decode_ref_data () =
  T.Node
    (T.make_node ~value:1l
       ~left:
         (T.Node
            (T.make_node ~value:2l ~left:(T.Empty 0l) ~right:(T.Empty 0l) ()))
       ~right:
         (T.Node
            (T.make_node ~value:3l ~left:(T.Empty 0l) ~right:(T.Empty 0l) ()))
       ())

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test08.c2ml.data" T.decode_pb_tree T.pp_tree
      (decode_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test08.ml2c.data" T.encode_pb_tree (decode_ref_data ())
