let () = 
  let r = Ocaml_types.({
    record_name = "test";
    fields = [ {
      field_type = Int; 
      field_name = "v1"; 
      type_qualifier = No_qualifier;
      encoding_type = Regular_field Encoding_util.({ 
        default = None; field_number = 1; nested = false; payload_kind = Encoding_util.Varint false}
      )
    }; {
      field_type = String; 
      field_name = "v2"; 
      type_qualifier = Option;
      encoding_type = Regular_field Encoding_util. ({ 
        default = None; field_number = 2; nested = false; payload_kind = Encoding_util.Bytes}
      )
    };{
      field_type = User_defined_type {type_name = "other"; module_ = None; }; 
      field_name = "v3"; 
      type_qualifier = No_qualifier;
      encoding_type = Regular_field Encoding_util.({ 
        default = None; field_number = 3; nested = true ; payload_kind = Encoding_util.Bytes}
      )
    };];
  }) in

  let s = {|type test = {
  mutable v1 : int;
  mutable v2 : string option;
  mutable v3 : other;
}|} in 
  assert(s = Ocaml_codegen.gen_type (Ocaml_types.{module_ = "A"; spec = Record r}));
  ()

let () = 
  print_endline "OCaml Codegen Test ... Ok"
