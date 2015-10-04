
module Pc = Protobuf_codec 
module T  = Test07_pb 


let decode_ref_data () = {
  T.value = 1; 
  T.left  = {T.t =  T.Node {T.value = 2; T.left = {T.t=T.Empty 0 }; T.right = {T.t = T.Empty 0}}}; 
  T.right = {T.t =  T.Node {T.value = 3; T.left = {T.t=T.Empty 0 }; T.right = {T.t = T.Empty 0}}}; 
} 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test07.c2ml.data" T.decode_node T.string_of_node (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test07.ml2c.data" T.encode_node (decode_ref_data ())

