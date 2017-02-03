let file_name = "a.proto" 

let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)
  
module Tt_util = Pb_typing_util 
module Tt = Pb_typing_type_tree
  
let file_options = Pb_option.empty

let () = 
  let s = "\
  message TestM { \
    enum TestE {\
      TestE_Value1 = 1; \
      TestE_Value2 = 2; \
    }\
    required TestE teste_field = 1; \
  }"
  in 
  let m = parse Pb_parsing_parser.message_ s in 
  let all_types = 
    Pb_typing_validation.validate_message 
        file_name file_options Tt_util.empty_scope m 
  in 
  assert (2 =List.length all_types); 
  match List.nth all_types 0 with 
  | {Tt.scope; spec = Tt.Enum {
      Tt.enum_name;
      Tt.enum_values; _;
  }; _;} -> (
    assert ("TestE" = enum_name); 
    assert (2 = List.length enum_values); 
    assert ({Tt.packages = []; Tt.message_names = ["TestM"]} = scope);
    ()
  )
  | _ -> (assert false:unit)

let () = 
  let test_fields message_body = 
    let f1 = List.nth message_body 0 in 
    let f1 = match f1 with 
      | Tt.Message_field f -> f
      | _ -> assert(false)
    in 
    assert (Tt_util.field_name f1 = "ival"); 
    assert (f1.Tt.field_type  = `Int64);
    assert (Tt_util.field_number f1 = 1); 
    assert (None = f1.Tt.field_default); 
    
    let f2 = List.nth message_body 1 in 
    let f2 = match f2 with 
      | Tt.Message_field f -> f
      | _ -> assert(false)
    in 
    assert (Tt_util.field_name f2 = "sval"); 
    assert (f2.Tt.field_type  = `String);
    assert (Tt_util.field_number f2 = 2); 
    assert (None = f2.Tt.field_default); 
    ()
  in 
  
  let s = "\
  message Test {\
    required int64 ival  = 1;\
    required string sval = 2;\
  }"
  in 
  let ast  = parse Pb_parsing_parser.message_ s in 
  let all_messages = 
    Pb_typing_validation.validate_message 
        file_name file_options Tt_util.empty_scope ast 
  in  
  assert (List.length all_messages = 1);
  begin
  match List.hd all_messages with
  | {Tt.scope; spec = Tt.Message {
      Tt.message_name;
      Tt.message_body; _; 
    }; _} -> (
    assert (Tt_util.empty_scope = scope);
    assert ("Test" = message_name); 
    assert (2 = List.length message_body); 
    
    test_fields message_body; 
    )
  | _ -> (assert false : unit) 
  end; 

  let s = "\
  message Test {\
    message Inner {\
      required int64 ival  = 1;\
      required string sval = 2;\
    }\
  }"
  in 
  let ast  = parse Pb_parsing_parser.message_ s in 
  let all_messages = 
    Pb_typing_validation.validate_message 
        "a.proto" file_options Tt_util.empty_scope ast 
  in  
  assert (List.length all_messages = 2);
  match List.hd all_messages with
  | {Tt.scope; spec = Tt.Message {
      Tt.message_name;
      Tt.message_body; _; 
    }; _} -> (
    assert (1 = List.length scope.Tt.message_names);
    assert ("Inner" = message_name); 
    assert (2 = List.length message_body); 
    test_fields message_body; 
    let expected_scope = {
      Tt.packages = []; 
      Tt.message_names = [ "Test" ] 
    } in 
    assert(expected_scope = scope);
    match List.nth all_messages 1 with
    | {Tt.scope; spec = Tt.Message {
        Tt.message_name;
        Tt.message_body; _; 
      }; _ } -> (
      assert (Tt_util.empty_scope = scope);
      assert ("Test" = message_name); 
      assert (0 = List.length message_body); 
      ()
    )
    | _ -> (assert false : unit)  
  )
  | _ -> (assert false : unit) 

let () = 
  let s = "\
  message Test {\
    required Msg1.Msg2.SubMessage mval  = 1;\
  }"
  in 
  let ast  = parse Pb_parsing_parser.message_ s in 
  let all_messages = 
    Pb_typing_validation.validate_message 
        "a.proto" file_options Tt_util.empty_scope ast 
  in  
  assert (List.length all_messages = 1);
  match List.hd all_messages with
  | {Tt.scope; spec = Tt.Message {
      Tt.message_name;
      Tt.message_body; _; 
    }; _ } -> (
    assert (Tt_util.empty_scope  = scope);
    assert ("Test" = message_name); 
    assert (1 = List.length message_body); 
    let f1 = List.nth message_body 0 in 
    let f1 = match f1 with | Tt.Message_field f -> f | _ -> assert(false) in 
    assert ("mval" = Tt_util.field_name f1); 
    assert (1 = Tt_util.field_number f1); 
    let unresolved = {
      Pb_field_type.type_path = ["Msg1";"Msg2"];
      Pb_field_type.type_name = "SubMessage";
      Pb_field_type.from_root = false;
    } in 
    assert ((`User_defined unresolved) = f1.Tt.field_type); 
    ()
  ) 
  | _ -> (assert false : unit)

let () = 
  print_endline "Pbtt Compile P1 ... Ok"
