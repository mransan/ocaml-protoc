
module T  = Test01_pb


let decode_ref_data () = {
    T.p1 = {
      T.first_name = "John";
      T.last_name  = "Doe";
      T.date_of_birth = 19820429; 
      T.tel_number = None; 
      T.employment = T.Employed_by "Google";
      T.marital_status = None; 
      T.gender = Some T.Male;
    }; 
    T.p2 = {
      T.first_name = "Marie";
      T.last_name  = "Dupont";
      T.date_of_birth = 19820306; 
      T.tel_number = Some {T.area_code = 917; T.number = 1111111};
      T.employment = T.Employed_by "INRIA";
      T.marital_status = None;
      T.gender = Some T.Female;
    };
    T.contact_numbers = {
      T.area_code = 917;
      T.number    = 123450;
    } :: {
      T.area_code = 917;
      T.number    = 123451;
    } :: []; 
    T.number_of_children = None; 
  } 

   

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test01.c2ml.data" T.decode_couple T.string_of_couple (decode_ref_data  ()) 
  | Test_util.Encode -> 
      Test_util.encode "test01.ml2c.data" T.encode_couple (decode_ref_data ())

