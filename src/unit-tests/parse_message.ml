let parse f s  = 
  f Lexer.lexer (Lexing.from_string s)

let () = 
  let s = "
  message Outer {
    required int64 ival = 1;
    required string sval = 2;
    
    message Inner { 
      required int64 inner_ival = 1;
      required string inner_sval = 2;
    }
    
    required Inner inner = 3; 
  }"
  in 
  (*
  Printf.printf "---- MESSAGE ----\n";
  *)
  let {
    Pbpt.message_name; 
    Pbpt.message_body;
  } = parse Parser.message_ s in 
  assert (message_name  = "Outer");
  assert (List.length message_body= 4);
  ()

let () = 
  let s = "
  message TestM { 
    enum TestE {
      TestE_Value1 = 1; 
      TestE_Value2 = 2; 
    }
    required TestE teste_field = 1; 
  }"
  in 
  (*
  Printf.printf "---- MESSAGE ----\n";
  *)
  let {
    Pbpt.message_name; 
    Pbpt.message_body;
  } = parse Parser.message_ s in 
  assert (message_name  = "TestM");
  assert (List.length message_body= 2);
  let (Pbpt.Message_enum {
    Pbpt.enum_name;
    Pbpt.enum_values; 
  }) = List.hd message_body in 
  assert ("TestE" = enum_name); 
  assert (2 = List.length enum_values);
  ()

let () = 
  let s =" 
  message Test {
  }" in 
  let proto = parse Parser.proto_ s in 
  let messages = proto.Pbpt.messages in 
  assert(1 = List.length messages);
  ()

let () = 
  let s =" 
  message M1 {} 
  message M2 {}
  " in 
  let proto = parse Parser.proto_ s in 
  let messages = proto.Pbpt.messages in 
  assert(2= List.length messages);
  assert("M1" = (List.nth messages 0).Pbpt.message_name);
  assert("M2" = (List.nth messages 1).Pbpt.message_name);
  ()

let () = 
  let s =" 
  package my.proto;
  message M1 {} 
  message M2 {}
  " in 
  let proto = parse Parser.proto_ s in 
  assert(Some "my.proto" = proto.Pbpt.package);
  assert(0= List.length proto.Pbpt.imports);
  assert(2= List.length proto.Pbpt.messages);
  assert("M1" = (List.nth proto.Pbpt.messages 0).Pbpt.message_name);
  assert("M2" = (List.nth proto.Pbpt.messages 1).Pbpt.message_name);
  ()

let () = 
  let s =" 
  import \"blah.proto\";
  import public \"boom.proto\";
  package my.proto;
  message M1 {} 
  message M2 {}
  " in 
  let proto = parse Parser.proto_ s in 
  assert(Some "my.proto" = proto.Pbpt.package);
  assert(2= List.length proto.Pbpt.imports);
  assert((Pbpt_util.import "blah.proto") = (List.nth proto.Pbpt.imports 0)); 
  assert(2= List.length proto.Pbpt.messages);
  assert("M1" = (List.nth proto.Pbpt.messages 0).Pbpt.message_name);
  assert("M2" = (List.nth proto.Pbpt.messages 1).Pbpt.message_name);
  ()

let () = 
  print_endline "Parse Message ... Ok"
