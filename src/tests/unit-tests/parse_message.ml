module E = Pb_exception
module Pt = Pb_parsing_parse_tree 

let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)

let () = 
  let s = "\
  message Outer {\
    required int64 ival = 1;\
    required string sval = 2;\
    \
    message Inner { \
      required int64 inner_ival = 1;\
      required string inner_sval = 2;\
    } ; /* make sure that the ';' is supported. */\
    \
    required Inner inner = 3; \
  }"
  in 
  (*
  Printf.printf "---- MESSAGE ----\n";
  *)
  let {
    Pt.message_name; 
    Pt.message_body; _;
  } = parse Pb_parsing_parser.message_ s in 
  assert (message_name  = "Outer");
  assert (List.length message_body= 4);
  ()

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
  (*
  Printf.printf "---- MESSAGE ----\n";
  *)
  let {
    Pt.message_name; 
    Pt.message_body; _;
  } = parse Pb_parsing_parser.message_ s in 
  assert (message_name  = "TestM");
  assert (List.length message_body= 2);
  match List.hd message_body with 
  | Pt.Message_enum {
      Pt.enum_name;
      Pt.enum_body; 
      Pt.enum_id = _; 
    } -> ( 
    assert ("TestE" = enum_name); 
    assert (2 = List.length enum_body);
    ()) 
  | _ -> (assert false : unit) ; 
  ()

let () = 
  let do_test s = 
    let proto = parse Pb_parsing_parser.proto_ s in 
    let messages = proto.Pt.messages in 
    assert(1 = List.length messages);
  in
  do_test " message Test { } ";
  do_test " message Test { }; ";
  do_test " message Test { } ; ; ;; ";
  ()


let () = 
  let s =" \
  message M1 {} \
  message M2 {}\
  " in 
  let proto = parse Pb_parsing_parser.proto_ s in 
  let messages = proto.Pt.messages in 
  assert(2= List.length messages);
  assert("M1" = (List.nth messages 0).Pt.message_name);
  assert("M2" = (List.nth messages 1).Pt.message_name);
  ()

let () = 
  let s =" \
  package my.proto;\
  message M1 {} \
  message M2 {}\
  " in 
  let proto = parse Pb_parsing_parser.proto_ s in 
  assert(None = proto.Pt.syntax);
  assert(Some "my.proto" = proto.Pt.package);
  assert(0= List.length proto.Pt.imports);
  assert(2= List.length proto.Pt.messages);
  assert("M1" = (List.nth proto.Pt.messages 0).Pt.message_name);
  assert("M2" = (List.nth proto.Pt.messages 1).Pt.message_name);
  ()

let () = 
  let s =" \
  syntax = \"proto2\";\
  import \"blah.proto\";\
  import public \"boom.proto\";\
  package my.proto;\
  message M1 {} \
  message M2 {}\
  extend  M2 {} \
  " in 
  let proto = parse Pb_parsing_parser.proto_ s in 
  assert(Some "proto2"  = proto.Pt.syntax);
  assert(Some "my.proto" = proto.Pt.package);
  assert(2= List.length proto.Pt.imports);
  assert((Pb_parsing_util.import "blah.proto") = (List.nth proto.Pt.imports 0)); 
  assert(2= List.length proto.Pt.messages);
  assert("M1" = (List.nth proto.Pt.messages 0).Pt.message_name);
  assert("M2" = (List.nth proto.Pt.messages 1).Pt.message_name);
  assert(1= List.length proto.Pt.extends);
  assert("M2" = (List.nth proto.Pt.extends 0).Pt.extend_name);
  ()

let () = 
  let s = "\
  message M {\
    required int32 ival = 1;\
  message M2 {}  message M3 {} message M4 {}\
  "
  in 
  match parse Pb_parsing_parser.message_ s with 
  | _ -> assert false 
  | exception E.Compilation_error _ -> ()
  | exception Parsing.Parse_error -> ()
  | exception exn -> print_endline @@ Printexc.to_string exn ; assert false 

  (* 
let () = 
  let s = "
  message M {
    oneof { 
      int32 i1 = 1; 
      int32 i2 = 2; 
    }
  }
  "
  in 
  match parse Pb_parsing_parser.message_ s with 
  | _ -> assert false 
  | exception exn -> 
    let s = Printexc.to_string exn  in 
    assert ("Missing oneof name" = s) 
*)

let () = 
  print_endline "Parse Message ... Ok"

let () = 
  print_endline "Parse Message ... Ok"
