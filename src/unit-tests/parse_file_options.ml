let parse s  = 
  Parser.file_option_list_ Lexer.lexer (Lexing.from_string s)


let () = 
  let s  = "option blah = 1;" in  
  let fo = ("blah", Pbpt.Constant_int 1)::[] in 
  assert (fo = parse s)  

let () = 
  let s  = "option blah = 1; option foo = \"blah\"; " in  
  let fo = ("blah", Pbpt.Constant_int 1)::
           ("foo" , Pbpt.Constant_string "blah")::[] in 
  assert (fo = parse s)  

let () =
  print_endline "Parse File Options ... Ok"
