let file_name = "a.proto" 

let parse f s  = 
  f Pblexer.lexer (Lexing.from_string s)
  
let file_options = []

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
  let m = parse Pbparser.message_ s in 
  let all_types = Pbtt_util.compile_message_p1 file_name file_options Pbtt_util.empty_scope m in 
  assert (2 =List.length all_types); 
  match List.nth all_types 0 with 
  | {Pbtt.scope; spec = Pbtt.Enum {
      Pbtt.enum_name;
      Pbtt.enum_values; _;
  }; _;} -> (
    assert ("TestE" = enum_name); 
    assert (2 = List.length enum_values); 
    assert ({Pbtt.packages = []; Pbtt.message_names = ["TestM"]} = scope);
    ()
  )
  | _ -> (assert false:unit)

let () = 
  let test_fields message_body = 
    let f1 = List.nth message_body 0 in 
    let f1 = match f1 with 
      | Pbtt.Message_field f -> f
      | _ -> assert(false)
    in 
    assert (Pbtt_util.field_name f1 = "ival"); 
    assert (f1.Pbtt.field_type  = Pbtt.Field_type_int64);
    assert (Pbtt_util.field_number f1 = 1); 
    assert (None = f1.Pbtt.field_default); 
    
    let f2 = List.nth message_body 1 in 
    let f2 = match f2 with 
      | Pbtt.Message_field f -> f
      | _ -> assert(false)
    in 
    assert (Pbtt_util.field_name f2 = "sval"); 
    assert (f2.Pbtt.field_type  = Pbtt.Field_type_string);
    assert (Pbtt_util.field_number f2 = 2); 
    assert (None = f2.Pbtt.field_default); 
    ()
  in 
  
  let s = "\
  message Test {\
    required int64 ival  = 1;\
    required string sval = 2;\
  }"
  in 
  let ast  = parse Pbparser.message_ s in 
  let all_messages = Pbtt_util.compile_message_p1 file_name file_options Pbtt_util.empty_scope ast in  
  assert (List.length all_messages = 1);
  begin
  match List.hd all_messages with
  | {Pbtt.scope; spec = Pbtt.Message {
      Pbtt.message_name;
      Pbtt.message_body; _; 
    }; _} -> (
    assert (Pbtt_util.empty_scope = scope);
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
  let ast  = parse Pbparser.message_ s in 
  let all_messages = Pbtt_util.compile_message_p1 "a.proto" file_options Pbtt_util.empty_scope ast in  
  assert (List.length all_messages = 2);
  match List.hd all_messages with
  | {Pbtt.scope; spec = Pbtt.Message {
      Pbtt.message_name;
      Pbtt.message_body; _; 
    }; _} -> (
    assert (1 = List.length scope.Pbtt.message_names);
    assert ("Inner" = message_name); 
    assert (2 = List.length message_body); 
    test_fields message_body; 
    let expected_scope = {
      Pbtt.packages = []; 
      Pbtt.message_names = [ "Test" ] 
    } in 
    assert(expected_scope = scope);
    match List.nth all_messages 1 with
    | {Pbtt.scope; spec = Pbtt.Message {
        Pbtt.message_name;
        Pbtt.message_body; _; 
      }; _ } -> (
      assert (Pbtt_util.empty_scope = scope);
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
  let ast  = parse Pbparser.message_ s in 
  let all_messages = Pbtt_util.compile_message_p1 "a.proto" file_options Pbtt_util.empty_scope ast in  
  assert (List.length all_messages = 1);
  match List.hd all_messages with
  | {Pbtt.scope; spec = Pbtt.Message {
      Pbtt.message_name;
      Pbtt.message_body; _; 
    }; _ } -> (
    assert (Pbtt_util.empty_scope  = scope);
    assert ("Test" = message_name); 
    assert (1 = List.length message_body); 
    let f1 = List.nth message_body 0 in 
    let f1 = match f1 with | Pbtt.Message_field f -> f | _ -> assert(false) in 
    assert ("mval" = Pbtt_util.field_name f1); 
    assert (1 = Pbtt_util.field_number f1); 
    let unresolved = {
      Pbtt.scope     = ["Msg1";"Msg2"];
      Pbtt.type_name = "SubMessage";
      Pbtt.from_root = false;
    } in 
    assert ((Pbtt.Field_type_type unresolved) = f1.Pbtt.field_type); 
    ()
  ) 
  | _ -> (assert false : unit)

let () = 
  let s = "\
  message M1 { \
    message M2 { message M21 { } } \
    message M3 { message M31 { message M311 { } } } \
  }\
  " in 
  let ast = parse Pbparser.message_ s in 
  let all_messages = Pbtt_util.compile_message_p1 "a.proto" file_options Pbtt_util.empty_scope  ast in 
  assert (6 = List.length all_messages); 
  let filtered = Pbtt_util.find_all_types_in_field_scope all_messages [] in 
  assert (1 = List.length filtered);
  let filtered = Pbtt_util.find_all_types_in_field_scope all_messages ["M1"] in 
  assert (2 = List.length filtered);
  let filtered = Pbtt_util.find_all_types_in_field_scope all_messages ["M1";"M2"] in 
  assert (1 = List.length filtered);
  let filtered = Pbtt_util.find_all_types_in_field_scope all_messages ["M1";"M3"] in 
  assert (1 = List.length filtered);
  let filtered = Pbtt_util.find_all_types_in_field_scope all_messages ["M1";"M3";"M31"] in 
  assert (1 = List.length filtered);
  ()

let () = 
  print_endline "Pbtt Compile P1 ... Ok"
