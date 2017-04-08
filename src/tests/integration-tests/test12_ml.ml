module T  = Test12_types
module Pb = Test12_pb
module Pp = Test12_pp

let decode_ref_data () = T.({
  to_ = 1l; 
  type_ = 1l;
}) 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test12.c2ml.data" Pb.decode_m  Pp.pp_m (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test12.ml2c.data" Pb.encode_m (decode_ref_data ())

