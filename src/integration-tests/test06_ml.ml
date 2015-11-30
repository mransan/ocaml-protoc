module T  = Test06_pb 


let decode_ref_data () = {
  T.teste_field = T.Teste_value1;
}

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> ( 
      Test_util.decode "test06.c2ml.data" T.decode_testm T.string_of_testm (decode_ref_data ()) 
  )
  | Test_util.Encode -> 
      Test_util.encode "test06.ml2c.data" T.encode_testm (decode_ref_data ())
