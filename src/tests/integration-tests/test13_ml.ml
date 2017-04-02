module T  = Test13_types
module Pb = Test13_pb
module Pp = Test13_pp

let decode_ref_data () = T.({
  p2 = {
    empty = ();
    sub = Sub_empty;
  };
  p1 = {
    l  = [Empty; Int 1l; Empty; Int 2l];
  }; 
}) 
  
let mode   = Test_util.parse_args ()

let () = 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode "test13.c2ml.data" Pb.decode_t Pp.pp_t (decode_ref_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test13.ml2c.data" Pb.encode_t (decode_ref_data ())
