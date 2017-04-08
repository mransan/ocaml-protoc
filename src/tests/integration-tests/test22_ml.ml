module T  = Test22_types
module Pb = Test22_pb
module Pp = Test22_pp

let decode_ref_data () = T.({
    p1 = Some {
      first_name = "John";
      last_name  = "Doe";
      date_of_birth = 19820429l; 
      tel_number = None;
      employment = Employed_by "Google";
      marital_status = Single; 
      gender = Male;
    }; 
    p2 = Some {
      first_name = "Marie";
      last_name  = "Dupont";
      date_of_birth = 19820306l; 
      tel_number = Some {area_code = 917l; number = 1111111l};
      employment = Employed_by "INRIA";
      marital_status = Married;
      gender = Female;
    };
    contact_numbers = {
      area_code = 917l;
      number    = 123450l;
    } :: {
      area_code = 917l;
      number    = 123451l;
    } :: []; 
    number_of_children = 0l;
  }) 

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test22.c2ml.data" Pb.decode_couple Pp.pp_couple (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test22.ml2c.data" Pb.encode_couple (decode_ref_data ())
let () = 

  let expected_default_person = T.({
    first_name = "";
    last_name = "";
    date_of_birth  = 0l;
    tel_number = None;
    employment = Self_employed 0l; 
    marital_status = Single; 
    gender = Male;
  }) in
  assert (expected_default_person = T.default_person ())

