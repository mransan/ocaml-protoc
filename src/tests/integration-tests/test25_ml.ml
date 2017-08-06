module T  = Test25_types
module Pb = Test25_pb
module Pp = Test25_pp

let decode_ref_data () = T.({f1 = [1l;2l;3l;4l] }) 
  
let mode   = Test_util.parse_args ()

let () = 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode "test25.c2ml.data" Pb.decode_m Pp.pp_m 
        (decode_ref_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test25.ml2c.data" Pb.encode_m (decode_ref_data ())
