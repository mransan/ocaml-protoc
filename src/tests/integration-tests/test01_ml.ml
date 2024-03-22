module T = Test01

let decode_ref_data () =
  T.
    {
      p1 =
        {
          first_name = "John";
          last_name = "Doe";
          date_of_birth = 19820429l;
          tel_number = None;
          employment = Some (Employed_by "Google");
          marital_status = None;
          gender = Some Male;
        };
      p2 =
        {
          first_name = "Marie";
          last_name = "Dupont";
          date_of_birth = 19820306l;
          tel_number = Some { area_code = 917l; number = 1111111l };
          employment = Some (Employed_by "INRIA");
          marital_status = None;
          gender = Some Female;
        };
      contact_numbers =
        [
          { area_code = 917l; number = 123450l };
          { area_code = 917l; number = 123451l };
        ];
      number_of_children = Some 0l;
    }

let () =
  Printf.printf "Show is working: %s\n" @@ T.show_couple (decode_ref_data ())

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test01.c2ml.data" T.decode_pb_couple T.pp_couple
      (decode_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test01.ml2c.data" T.encode_pb_couple (decode_ref_data ())

let () =
  let expected_default_person =
    T.
      {
        first_name = "Max";
        last_name = "Ransan";
        date_of_birth = 19820429l;
        tel_number = None;
        employment = None;
        marital_status = None;
        gender = None;
      }
  in
  assert (expected_default_person = T.default_person ())
