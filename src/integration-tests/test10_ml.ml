module Pc = Protobuf_codec 
module T  = Test10_pb

let decode_ref_data () = {
    T.m10_f1 = {
      Test09_pb.int32_f1 = 1; 
    }; 
  } 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test10.c2ml.data" T.decode_m10  T.string_of_m10 (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test10.ml2c.data" T.encode_m10 (decode_ref_data ())

