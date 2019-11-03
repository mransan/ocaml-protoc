module E = Pb_exception

let parse s =
  Pb_parsing_parser.proto_ Pb_parsing_lexer.lexer (Lexing.from_string s)

(* invalid proto 2 field *) 

let () =
  let proto = parse "message M { string no_label = 1; }" in  
  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> assert(false) 
  | exception E.Compilation_error _ -> () 

(* invalid proto 2 field with setting the syntax explicitely *) 

let () =
  let proto = parse "\
    syntax = \"proto2\"; \
    message M { \
      string no_label = 1; \
    }" in  

  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> assert(false) 
  | exception E.Compilation_error _ -> () 

(* valid proto 3 field *) 

let () =
  let proto = parse "\
    syntax = \"proto3\"; \
    message M { \
      string no_label = 1; \
      repeated string r = 2; \
      message S {\
        string s_no_label = 3; \
        repeated string s_r = 4; \
      } \
    }" in  

  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> ()
  | exception E.Compilation_error _ -> assert(false)

(* invalid proto 3 field label *)

let () =
  let proto = parse "\
    syntax = \"proto3\"; \
    message M { \
      required string no_label = 1; \
    }" in  

  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> assert(false) 
  | exception E.Compilation_error _ -> () 

let () =
  let proto = parse "\
    syntax = \"proto3\"; \
    message M { message S { \
      required string no_label = 1; \
    }}" in  

  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> assert(false) 
  | exception E.Compilation_error _ -> () 

(* invalid proto3 first enum value *)

let () =
  let proto = parse "\
    syntax = \"proto3\"; \
    enum E { EN1 = 1; EN2 = 2; } \
    message M { } \
    " in  

  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> assert(false) 
  | exception E.Compilation_error _ -> () 

let () =
  let proto = parse "\
    syntax = \"proto3\"; \
    message M { \
      enum E { EN1 = 1; EN2 = 2; } \
    }" in  

  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> assert(false) 
  | exception E.Compilation_error _ -> () 
  

(* valid proto3 first enum value *) 

let () =
  let proto = parse "\
    syntax = \"proto3\"; \
    message M { \
      enum E { EN0 = 0; EN2 = 2; } \
    }" in  

  match Pb_parsing_util.finalize_proto_value proto with
  | _ -> () 
  | exception E.Compilation_error _ -> assert(false)

let () =
  print_endline "Verify syntax invariants ... Ok"
