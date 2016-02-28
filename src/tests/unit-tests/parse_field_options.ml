
let parse f s  = 
  f Pblexer.lexer (Lexing.from_string s)

let () = 

  let test_default s = 
    (*
    Printf.printf "---- %s ----\n" s;
    loop @@ Lexing.from_string s; 
     *)
    List.assoc "default" @@ parse Pbparser.field_options_ s  
  in 
  
  assert (Pbpt.Constant_int 1    = test_default "[default = 1 ]"); 
  assert (Pbpt.Constant_int (-1) = test_default "[default = -1]"); 
  assert (Pbpt.Constant_string "hello" = test_default "[default = \"hello\"]"); 
  assert (Pbpt.Constant_string "he'l'lo" = test_default "[default = \"he'l'lo\"]"); 
  assert (Pbpt.Constant_string "he\"l\"lo" = test_default "[default = \"he\\\"l\\\"lo\"]"); 
  assert (Pbpt.Constant_float 1.23 = test_default "[default = 1.23]"); 
  assert (Pbpt.Constant_float (-. 1.23) = test_default "[default = -1.23]"); 
  assert (Pbpt.Constant_bool true  = test_default "[default = true]"); 
  assert (Pbpt.Constant_bool false = test_default "[default = false]"); 
  () 

let () = 
  let field_options = parse Pbparser.field_options_ "[]" in 
  assert (field_options = [])

let () = 
  let field_options = parse Pbparser.field_options_ "[a=1,b=true]" in 
  assert (List.length field_options = 2)

let () = 
  let field_options = parse Pbparser.field_options_ "[(ocaml_type) = int]" in 
  assert ([("ocaml_type", Pbpt.Constant_litteral "int")] = field_options)

let () =
  print_endline "Parse Field Options ... Ok"
