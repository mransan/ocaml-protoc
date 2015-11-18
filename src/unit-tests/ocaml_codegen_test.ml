let () = 
  let r = Ocaml_types.({
    record_name = "test";
    fields = [ {
      field_type = Int; 
      field_name = "v1"; 
      type_qualifier = No_qualifier;
      encoding_type = Regular_field {field_number = 1; nested = false; payload_kind = Encoding_util.Varint false}
    }; {
      field_type = String; 
      field_name = "v2"; 
      type_qualifier = Option;
      encoding_type = Regular_field {field_number = 2; nested = false; payload_kind = Encoding_util.Bytes}
    };{
      field_type = User_defined_type "other"; 
      field_name = "v3"; 
      type_qualifier = No_qualifier;
      encoding_type = Regular_field {field_number = 3; nested = true ; payload_kind = Encoding_util.Bytes}
    };];
  }) in

  let s = {|type test = {
  v1 : int;
  v2 : string option;
  v3 : other;
}|} in 
  assert(s = Ocaml_codegen.gen_type (Ocaml_types.Record r));
  
  let s = {|let test_mappings = [
  (1, (fun d -> `Int (decode_varint_as_int d)));
  (2, (fun d -> `String (decode_bytes_as_string d)));
  (3, (fun d -> `Other (decode_other (Pc.Decoder.nested d))));
]|} in

  assert (s = Ocaml_codegen.gen_mappings_record r);
  ()

let () = 
  print_endline "OCaml Codegen Test ... Ok"
