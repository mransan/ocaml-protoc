
module T  = Test11_pb


let create_ref_data n =

  let rec loop l = function
    | 0 -> l 
    | n -> 
      let couple = { 
        T.p1 = {
          T.first_name = "John";
          T.last_name  = "Doe";
          T.date_of_birth = 19820429; 
          T.tel_number = None; 
          T.employment = T.Employed_by "Google";
          T.marital_status = None; 
        }; 
        T.p2 = {
          T.first_name = "Marie";
          T.last_name  = "Dupont";
          T.date_of_birth = 19820306; 
          T.tel_number = Some {T.area_code = 917; T.number = 1111111};
          T.employment = T.Employed_by "INRIA";
          T.marital_status = None;
        };
        T.contact_numbers = {
          T.area_code = 917;
          T.number    = 123450;
        } :: {
          T.area_code = 917;
          T.number    = 123451;
        } :: [];
        T.number_of_children = None; 
    } in
    loop (couple :: l) (n - 1)
  in 
  {T.all = loop [] n}   

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test11.c2ml.data" ~notest:() T.decode_couples
      T.string_of_couples (create_ref_data 100000)
  | Test_util.Encode -> 
      Test_util.encode "test11.ml2c.data" T.encode_couples (create_ref_data 1)

