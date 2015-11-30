module T  = Test09_pb

let decode_ref_data () = {
    T.int32_f1 = 1; 
  } 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test09.c2ml.data" T.decode_m09 T.string_of_m09 (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test09.ml2c.data" T.encode_m09 (decode_ref_data ())

