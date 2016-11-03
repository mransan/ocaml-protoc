module E = Pb_exception 

let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)

module Pt = Pb_parsing_parse_tree 
module Pt_util = Pb_parsing_util 

let () = 
  let s =" \
  ENUM1 = 1;\
  " in 
  begin match parse Pb_parsing_parser.enum_value_ s with
  | Pt.Enum_value ev -> 
    assert("ENUM1" = ev.Pt.enum_value_name);
    assert(1       = ev.Pt.enum_value_int)
  | _ -> assert(false)
  end; 
  ()

let () = 
  let s =" \
  BLAH_12_BLAH = -99999; ;\
  " in 
  begin match parse Pb_parsing_parser.enum_value_ s with
  | Pt.Enum_value ev -> 
    assert("BLAH_12_BLAH" = ev.Pt.enum_value_name);
    assert((-99999) = ev.Pt.enum_value_int);
  | _ -> assert(false)
  end; 
  ()

let () = 
  let s =" \
  enum Test {\
  EV1 = 1;\
  EV2 = 2;\
  }\
  " in 
  let ev1 = Pt_util.enum_value ~int_value:1 "EV1" in 
  let ev2 = Pt_util.enum_value ~int_value:2 "EV2" in 
  let e   = parse Pb_parsing_parser.enum_ s in 
  assert("Test" = e.Pt.enum_name); 
  assert(2 = List.length e.Pt.enum_body);
  assert(ev1 = List.nth e.Pt.enum_body 0); 
  assert(ev2 = List.nth e.Pt.enum_body 1); 
  ()

let () = 
  let s =" \
  enum Test {\
  EV1 = 1\
  EV2 = 2;\
  }\
  " in 
  match parse Pb_parsing_parser.enum_ s with
  | _ -> assert false 
  | exception E.Compilation_error _ -> ()
  | exception exn -> 
    print_endline @@ Printexc.to_string exn; 
    assert false

let () = 
  print_endline "Parse Enum ... Ok" 
