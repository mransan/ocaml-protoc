
let person = Example01_pb.({ 
  first_name = "John Doe"; 
  id = 1234l;
  email = "jdoe@example.com"; 
  child = {cute_name = "Booboo"}; 
  phone = ["917-929-8071"; "646-269-2829"];
  residence = House 12.;
}) 

let () = 
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
  Format.fprintf Format.std_formatter "debug:\n%a\n" 
                 Example01_pb.pp_person person 

module JsonEncoder = Example01_pb.Make_encoder(Pbrt_js_yojson.Encoder) 
module JsonDecoder = Example01_pb.Make_decoder(Pbrt_js_yojson.Decoder)

let () = 
  let encoder = Pbrt_js_yojson.Encoder.empty () in 
  JsonEncoder.encode_person person encoder; 
  let json_str = Yojson.Basic.to_string (`Assoc !encoder) in 
  print_endline "Json value:"; 
  print_endline json_str; 
  match Yojson.Basic.from_string json_str with 
  | `Assoc a -> 
     let decoder = ref a in
     let person' = JsonDecoder.decode_person decoder in 
     assert(person' = person)
  | _ -> assert(false)
