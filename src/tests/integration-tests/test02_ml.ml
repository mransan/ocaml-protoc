
module T  = Test02_types 
module Pp  = Test02_pp 
module Pb  = Test02_pb 

let decode_ref_data () = {
  T.o01 = 1.0;
  T.o02 = 2.0;
  T.o03 = (-123l);
  T.o04 = 456L;
  T.o05 = 123l;
  T.o06 = 456L; 
  T.o07 = (-123l);
  T.o08 = (-456L); 
  T.o09 = 0xFFFFFFFFl;
  T.o10 = 0xFFFFFFFFFFFFFFFFL;
  T.o11 = 0xFFFFFFFFl;
  T.o12 = 0xFFFFFFFFFFFFFFFFL;
  T.o13 = true;
  T.o14 = "Iam a test string";
  T.o15 = Bytes.of_string "Iam a test byte"; 
}

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test02.c2ml.data" Pb.decode_all_basics_types Pp.pp_all_basics_types (decode_ref_data ()) 
  | Test_util.Encode -> 
      Test_util.encode "test02.ml2c.data" Pb.encode_all_basics_types (decode_ref_data ())

