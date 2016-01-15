module T  = Test08_pb 

let decode_ref_data () = T.Node {
    T.value = 1; 
    T.left  = T.Node {T.value = 2; T.left = T.Empty 0; right = T.Empty 0};
    T.right = T.Node {T.value = 3; T.left = T.Empty 0; right = T.Empty 0};
  }


let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test08.c2ml.data" T.decode_tree T.pp_tree (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test08.ml2c.data" T.encode_tree (decode_ref_data ())
