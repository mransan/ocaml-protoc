module T = Test11

let create_ref_data n =
  let rec loop l = function
    | 0 -> l
    | n ->
      let couple =
        T.make_couple
          ~p1:
            (T.make_person ~first_name:"John" ~last_name:"Doe"
               ~date_of_birth:(Int32.of_int n)
               ~employment:(Employed_by "Google") ())
          ~p2:
            (T.make_person ~first_name:"Marie" ~last_name:"Dupont"
               ~date_of_birth:19820306l
               ~tel_number:
                 (T.make_person_tel_number ~area_code:917l ~number:1111111l ())
               ~employment:(Employed_by "INRIA") ())
          ~contact_numbers:
            [
              T.make_person_tel_number ~area_code:917l ~number:123450l ();
              T.make_person_tel_number ~area_code:917l ~number:123451l ();
            ]
          ()
      in
      loop (couple :: l) (n - 1)
  in
  T.make_couples ~all:(loop [] n) ()

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test11.c2ml.data" ~notest:() T.decode_pb_couples
      T.pp_couples (create_ref_data 100000)
  | Test_util.Encode ->
    Test_util.encode "test11.ml2c.data" T.encode_pb_couples
      (create_ref_data 100000)
