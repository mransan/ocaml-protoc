
let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)

module Pt = Pb_parsing_parse_tree 
let () = 

  let test_default s = 
    (*
    Printf.printf "---- %s ----\n" s;
    loop @@ Lexing.from_string s; 
     *)
    List.assoc "default" @@ parse Pb_parsing_parser.field_options_ s  
  in 
  
  assert (Pt.Constant_int 1    = test_default "[default = 1 ]"); 
  assert (Pt.Constant_int (-1) = test_default "[default = -1]"); 
  assert (Pt.Constant_string "hello" = test_default "[default = \"hello\"]"); 
  assert (Pt.Constant_string "he'l'lo" = test_default "[default = \"he'l'lo\"]"); 
  assert (Pt.Constant_string "he\"l\"lo" = test_default "[default = \"he\\\"l\\\"lo\"]"); 
  assert (Pt.Constant_float 1.23 = test_default "[default = 1.23]"); 
  assert (Pt.Constant_float (-. 1.23) = test_default "[default = -1.23]"); 
  assert (Pt.Constant_bool true  = test_default "[default = true]"); 
  assert (Pt.Constant_bool false = test_default "[default = false]"); 
  () 

let () = 
  let field_options = parse Pb_parsing_parser.field_options_ "[]" in 
  assert (field_options = [])

let () = 
  let field_options = parse Pb_parsing_parser.field_options_ "[a=1,b=true]" in 
  assert (List.length field_options = 2)

let () = 
  let field_options = parse Pb_parsing_parser.field_options_ "[(ocaml_type) = int]" in 
  assert ([("ocaml_type", Pt.Constant_litteral "int")] = field_options)

let () =
  print_endline "Parse Field Options ... Ok"
