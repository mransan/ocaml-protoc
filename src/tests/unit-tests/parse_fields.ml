
let parse f s  = 
  f Lexer.lexer (Lexing.from_string s)

let () =
  let {
    Pbpt.field_name; 
    field_type; 
    field_label; 
    field_options; 
    field_number
  } = parse Parser.normal_field_ "optional int32 x = 1 [default=1];" in 
  assert (field_name = "x"); 
  assert (field_label = `Optional); 
  assert (field_type = "int32"); 
  assert (field_number = 1); 
  assert (List.length field_options = 1)

let () =
  let {
    Pbpt.field_name; 
    field_type; 
    field_label; 
    field_options; 
    field_number
  } = parse Parser.normal_field_ "optional .M1 x = 1;" in 
  assert (field_name = "x"); 
  assert (field_label = `Optional); 
  assert (field_type = ".M1"); 
  assert (field_number = 1); 
  assert (List.length field_options = 0)

let () = 
  let s = "
    oneof foo {
      string name = 4;
      SubMessage sub_message = 9 [a=1];
    }"
  in 
  let {
    Pbpt.oneof_name; 
    Pbpt.oneof_fields; 
  } = parse Parser.oneof_ s in
  assert (oneof_name = "foo"); 
  assert (List.length oneof_fields = 2);
  let {
    Pbpt.field_name; 
    Pbpt.field_number; 
    Pbpt.field_type; 
    Pbpt.field_options; 
  } = List.nth oneof_fields 0 in 
  assert (field_name = "name"); 
  assert (field_type = "string"); 
  assert (field_number= 4); 
  assert (List.length field_options = 0); 
  let {
    Pbpt.field_name; 
    Pbpt.field_number; 
    Pbpt.field_type; 
    Pbpt.field_options; 
  } = List.nth oneof_fields 1 in 
  assert (field_name = "sub_message"); 
  assert (field_type = "SubMessage"); 
  assert (field_number= 9); 
  assert (List.length field_options = 1); 
  ()

let () =
  print_endline "Parse Fields ... Ok"

