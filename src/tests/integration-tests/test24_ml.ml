module T  = Test24_types
module Pb = Test24_pb
module Pp = Test24_pp

let decode_ref_data () = (T.Value "value" : T.a)

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test24.c2ml.data" Pb.decode_a  Pp.pp_a (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test24.ml2c.data" Pb.encode_a (decode_ref_data ())

