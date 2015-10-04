
let field ?options:(options =[]) ~label ~number ~type_ name = {
  Pbpt.field_name = name; 
  Pbpt.field_number = number;
  Pbpt.field_type = type_; 
  Pbpt.field_label = label;
  Pbpt.field_options = options;
} 

let oneof_field ?options:(options =[]) ~number ~type_ name = {
  Pbpt.field_name = name; 
  Pbpt.field_number = number;
  Pbpt.field_type = type_; 
  Pbpt.field_options = options;
  Pbpt.field_label = `Oneof
} 

let oneof ~fields name = {
  Pbpt.oneof_name = name;
  Pbpt.oneof_fields = fields;
}

let message_counter = ref 0

let enum_value ~int_value name = { 
  Pbpt.enum_value_name = name; 
  Pbpt.enum_value_int = int_value; 
}

let enum ?enum_values:(enum_values = []) enum_name =
  incr message_counter; 
  {
    Pbpt.enum_id  = !message_counter; 
    Pbpt.enum_name; 
    Pbpt.enum_values; 
  } 

let message_body_field field =  Pbpt.Message_field field  
let message_body_oneof_field field =  Pbpt.Message_oneof_field   field  
let message_body_sub message  =  Pbpt.Message_sub message 
let message_body_enum enum = Pbpt.Message_enum enum


let message ~content name = 
  incr message_counter;
  {
    Pbpt.id = !message_counter;
    Pbpt.message_name = name; 
    message_body = content;
  } 

let proto ?package messages = {
  Pbpt.package; 
  Pbpt.messages; 
}

let rec message_printer ?level:(level = 0) {
  Pbpt.message_name; 
  Pbpt.message_body; _ } = 

  let prefix () = 
    for _ =0 to level  - 1 do 
      Printf.printf " ";
    done; 
  in 
  prefix (); print_endline message_name; 
  List.iter (function 
    | Pbpt.Message_field {Pbpt.field_name; _ } -> 
        prefix (); Printf.printf "- field [%s]\n" field_name
    | Pbpt.Message_oneof_field {Pbpt.oneof_name ; _ } ->
        prefix (); Printf.printf "- one of field [%s]\n" oneof_name
    | Pbpt.Message_enum {Pbpt.enum_name; _ } ->
        prefix (); Printf.printf "- enum type [%s]\n" enum_name 
    | Pbpt.Message_sub m -> message_printer ~level:(level + 2) m
  ) message_body 
