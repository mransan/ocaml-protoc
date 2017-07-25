module T  = Test23_types 
module Pp  = Test23_pp 
module Pb  = Test23_pb 

let decode_ref_data () = {
  T.i = Int64.(shift_left 3L 56);
}

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test23.c2ml.data" Pb.decode_int64_t Pp.pp_int64_t (decode_ref_data ()) 
  | Test_util.Encode -> 
      Test_util.encode "test23.ml2c.data" Pb.encode_int64_t (decode_ref_data ())

