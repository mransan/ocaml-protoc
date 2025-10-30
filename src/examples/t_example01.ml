let () =
  (* Create OCaml value of generated type *)
  let person =
    Example01.make_person ~name:"John Doe" ~id:1234l ~email:"jdoe@example.com"
      ~phone:[ "123-456-7890" ] ()
  in

  print_endline @@ Format.asprintf "person: %a" Example01.pp_person person;

  (*
  let json = 
    person 
    |> Example01_yojson.encode_person 
    |> Yojson.Basic.to_string 
  in
  print_endline json; 

  let person' = 
    json
    |> Yojson.Basic.from_string
    |> Example01_yojson.decode_person
  in  

  assert(person = person');
  *)
  let binary =
    let encoder = Pbrt.Encoder.create () in
    Example01.encode_pb_person person encoder;
    Pbrt.Encoder.to_bytes encoder
  in

  let person' =
    let decoder = Pbrt.Decoder.of_bytes binary in
    Example01.decode_pb_person decoder
  in

  assert (person = person')
