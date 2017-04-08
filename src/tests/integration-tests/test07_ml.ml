module T  = Test07_types
module Pb = Test07_pb
module Pp = Test07_pp


let decode_ref_data () = {
  T.value = 1l; 
  T.left  = T.Node {T.value = 2l; T.left = T.Empty 0l ; T.right = T.Empty 0l}; 
  T.right = T.Node {T.value = 3l; T.left = T.Empty 0l ; T.right = T.Empty 0l}; 
} 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test07.c2ml.data" Pb.decode_node Pp.pp_node (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test07.ml2c.data" Pb.encode_node (decode_ref_data ())

