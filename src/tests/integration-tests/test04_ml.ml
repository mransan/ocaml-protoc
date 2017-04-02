module T  = Test04_types
module Pb = Test04_pb
module Pp = Test04_pp

let decode_ref_data () = {
  T.j = 456l;
}

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test03.c2ml.data" Pb.decode_test Pp.pp_test (decode_ref_data ()) 
  | Test_util.Encode -> 
      Test_util.encode "test04.ml2c.data" Pb.encode_test (decode_ref_data ())
