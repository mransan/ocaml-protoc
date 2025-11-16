module T = Test22

let decode_pb_ref_data () =
  T.make_couple
    ~p1:
      (T.make_person ~first_name:"John" ~last_name:"Doe"
         ~date_of_birth:19820429l ~employment:(Employed_by "Google")
         ~gender:Male ())
    ~p2:
      (T.make_person ~first_name:"Marie" ~last_name:"Dupont"
         ~date_of_birth:19820306l
         ~tel_number:
           (T.make_person_tel_number ~area_code:917l ~number:1111111l ())
         ~employment:(Employed_by "INRIA") ~marital_status:Married
         ~gender:Female ())
    ~contact_numbers:
      [
        T.make_person_tel_number ~area_code:917l ~number:123450l ();
        T.make_person_tel_number ~area_code:917l ~number:123451l ();
      ]
    ()

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test22.c2ml.data" T.decode_pb_couple T.pp_couple
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test22.ml2c.data" T.encode_pb_couple
      (decode_pb_ref_data ())
