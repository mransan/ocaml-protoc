module T  = Test06_types
module Pb = Test06_pb
module Pp = Test06_pp

let decode_ref_data () = {
  T.teste_field = T.Test_e_value1;
}

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> ( 
      Test_util.decode "test06.c2ml.data" Pb.decode_test_m Pp.pp_test_m (decode_ref_data ()) 
  )
  | Test_util.Encode -> 
      Test_util.encode "test06.ml2c.data" Pb.encode_test_m (decode_ref_data ())
