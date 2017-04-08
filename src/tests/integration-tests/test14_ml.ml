module T  = Test14_types
module Pb = Test14_pb
module Pp = Test14_pp

let decode_ref_data () = T.({
  da = {aa = [1l;2l;3l;4l] };
  db = Ba {aa = [1l;2l;3l;4l]; };
  dc = { 
    sub = Ca {aa = [1l;2l;3l;4l;]}; 
    cc  = Some {aa = [1l;2l;3l;4l;]}; 
  };  
}) 
  
let mode   = Test_util.parse_args ()

let () = 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode "test14.c2ml.data" Pb.decode_d Pp.pp_d (decode_ref_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test14.ml2c.data" Pb.encode_d (decode_ref_data ())
