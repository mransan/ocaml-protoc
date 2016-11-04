let parse s  = 
  Pb_parsing_parser.option_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let parse_proto s  = 
  Pb_parsing_parser.proto_ Pb_parsing_lexer.lexer (Lexing.from_string s)

module Pt = Pb_parsing_parse_tree

let () = 
  let s  = "option blah = 1;" in  
  let fo = ("blah", Pt.Constant_int 1) in 
  assert (fo = parse s)  

let () = 
  let s  = "option blah = 1; ;  option foo = \"blah\"; " in  
  let fo = ("blah", Pt.Constant_int 1)::
           ("foo" , Pt.Constant_string "blah")::[] in 
  let proto = parse_proto s in 
  let fo_parsed = proto.Pt.file_options in 
  assert (fo = fo_parsed)  

let () =
  print_endline "Parse File Options ... Ok"
