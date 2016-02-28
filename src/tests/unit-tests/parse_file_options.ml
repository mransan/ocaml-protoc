let parse s  = 
  Pbparser.file_option_ Pblexer.lexer (Lexing.from_string s)

let parse_proto s  = 
  Pbparser.proto_ Pblexer.lexer (Lexing.from_string s)


let () = 
  let s  = "option blah = 1;" in  
  let fo = ("blah", Pbpt.Constant_int 1) in 
  assert (fo = parse s)  

let () = 
  let s  = "option blah = 1; ;  option foo = \"blah\"; " in  
  let fo = ("blah", Pbpt.Constant_int 1)::
           ("foo" , Pbpt.Constant_string "blah")::[] in 
  let proto = parse_proto s in 
  let fo_parsed = proto.Pbpt.file_options in 
  assert (fo = fo_parsed)  

let () =
  print_endline "Parse File Options ... Ok"
