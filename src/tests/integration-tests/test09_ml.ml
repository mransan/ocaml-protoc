module T  = Test09_types
module Pb = Test09_pb
module Pp = Test09_pp

let decode_ref_data () = {
    T.int32_f1 = 1l; 
  } 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test09.c2ml.data" Pb.decode_m09 Pp.pp_m09 (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test09.ml2c.data" Pb.encode_m09 (decode_ref_data ())

