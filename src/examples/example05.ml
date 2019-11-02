let () =
  (* Create OCaml value of generated type *)

  let details = Hashtbl.create 100 in 
  Hashtbl.add details "foo" "bar"; 
  Hashtbl.add details "blah" "blah";

  let person = Example05_types.({
    name = "John Doe";
    id = 1234l;
    email = Some "jdoe@example.com";
    phone = ["123-456-7890"];
    details;
  }) in

  (* Create a Protobuf encoder and encode value *)

  let encoder = Pbrt.Encoder.create () in
  Example05_pb.encode_person person encoder;

  (* Output the protobuf message to a file *)

  let oc = open_out "myfile" in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc

let () =
  let bytes =
    let ic = open_in "myfile" in
    let len = in_channel_length ic in
    let bytes = Bytes.create len in
    really_input ic bytes 0 len;
    close_in ic;
    bytes
  in
  let person = Example05_pb.decode_person (Pbrt.Decoder.of_bytes bytes) in
  Format.fprintf Format.std_formatter "%a" Example05_pp.pp_person person

