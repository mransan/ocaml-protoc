
module T  = Test27_types
module Pb = Test27_pb
module Pp = Test27_pp

let test_data () = T.({
    enum1 = Value_with_option;
    unit_= Some { unit_field = 1l};
    one_of_is_keyword = Some (One_of_is_keyword_field 1l);
  }) 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode 
        "test27.c2ml.data" Pb.decode_test Pp.pp_test (test_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test27.ml2c.data" Pb.encode_test (test_data ())
