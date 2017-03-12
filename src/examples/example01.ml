let () = 
  (* Create OCaml value of generated type *) 

  let person = Example01_pb.({ 
    first_name = "John Doe"; 
    id = 1234l;
    email = "jdoe@example.com"; 
    child = {cute_name = "Booboo"}; 
(*    phone = ["123-456-7890"];*)
  }) in 
  
  (* Create a Protobuf encoder and encode value *)

  let encoder = Pbrt.Encoder.create () in 
  Example01_pb.encode_person person encoder; 

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
  
  let person = Example01_pb.decode_person (Pbrt.Decoder.of_bytes bytes) in 
  Format.fprintf Format.std_formatter "%a" Example01_pb.pp_person person 
