module T  = Test10_types
module Pb = Test10_pb
module Pp = Test10_pp

let decode_ref_data () = {
    T.m10_f1 = {
      Test09_types.int32_f1 = 1l; 
    }; 
  } 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test10.c2ml.data" Pb.decode_m10  Pp.pp_m10 (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test10.ml2c.data" Pb.encode_m10 (decode_ref_data ())

