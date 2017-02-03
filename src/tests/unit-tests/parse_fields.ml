
let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)

let has_option set option_name = 
  match Pb_option.get set option_name with
  | None -> false
  | Some _ -> true

module Pt = Pb_parsing_parse_tree 

let () =
  let do_test s = 
    let {
      Pt.field_name; 
      field_type; 
      field_label; 
      field_options; 
      field_number
    } = parse Pb_parsing_parser.normal_field_ s in 
    assert (field_name = "x"); 
    assert (field_label = `Optional); 
    assert (field_type = `Int32);
    assert (field_number = 1); 
    assert (has_option field_options "default"); 
  in
  do_test "optional int32 x = 1 [default=1];"; 
  do_test "optional int32 x = 1 [default=1] ; ; ;; "; 
  ()

let () =
  let {
    Pt.field_name; 
    field_type; 
    field_label; 
    field_options; 
    field_number
  } = parse Pb_parsing_parser.normal_field_ "optional .M1 x = 1;" in 
  assert (field_name = "x"); 
  assert (field_label = `Optional); 

  assert (field_type = `User_defined {
    Pb_field_type.type_path = [];
    Pb_field_type.type_name = "M1"; 
    Pb_field_type.from_root = true; 
  });
  assert (field_number = 1); 
  assert (field_options = Pb_option.empty)

let () = 

  let do_test s =  
    let {
      Pt.oneof_name; 
      Pt.oneof_fields; 
    } = parse Pb_parsing_parser.oneof_ s in
    assert (oneof_name = "foo"); 
    assert (List.length oneof_fields = 2);
    let {
      Pt.field_name; 
      Pt.field_number; 
      Pt.field_type; 
      Pt.field_options; _ 
    } = List.nth oneof_fields 0 in 
    assert (field_name = "name"); 
    assert (field_type = `String); 
    assert (field_number= 4); 
    assert (field_options = Pb_option.empty);
    let {
      Pt.field_name; 
      Pt.field_number; 
      Pt.field_type; 
      Pt.field_options; _; 
    } = List.nth oneof_fields 1 in 
    assert (field_name = "sub_message"); 
    assert (field_type = `User_defined {
      Pb_field_type.type_path = []; 
      Pb_field_type.type_name ="SubMessage"; 
      Pb_field_type.from_root = false;
    });
    assert (field_number= 9); 
    assert (has_option field_options "a"); 
    ()
  in

  do_test "\
    oneof foo {\
      string name = 4;\
      SubMessage sub_message = 9 [a=1];\
    }"; 

  (* below test is to check resilience with respect to 
   * semi colon
   *)
  do_test "\
    oneof foo {\
      string name = 4; ; \
      SubMessage sub_message = 9 [a=1]; ; ;;\
    }; ;; ";
  ()

let () =
  print_endline "Parse Fields ... Ok"

