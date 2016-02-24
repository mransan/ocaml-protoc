
module T  = Test02_pb 

let decode_ref_data () = {
  T.o01 = 1.0;
  T.o02 = 2.0;
  T.o03 = (-123l);
  T.o04 = 456L;
  T.o05 = 123l;
  T.o06 = 456L; 
  T.o07 = (-123l);
  T.o08 = (-456L); 
  T.o09 = 123l;
  T.o10 = 456L;
  (*
  T.o11 ;
  T.o12 ;
  *)
  T.o13 = true;
  T.o14 = "Iam a test string";
  T.o15 = "Iam a test byte"; 
}

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test02.c2ml.data" T.decode_all_basics_types T.pp_all_basics_types (decode_ref_data ()) 
  | Test_util.Encode -> 
      Test_util.encode "test02.ml2c.data" T.encode_all_basics_types (decode_ref_data ())

