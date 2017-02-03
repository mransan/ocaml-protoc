let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)
  
let has_option set option_name = 
  match Pb_option.get set option_name with
  | None -> false
  | Some _ -> true

module Pt = Pb_parsing_parse_tree 
let () = 

  let test_default s = 
    Pb_option.get (parse Pb_parsing_parser.field_options_ s) "default"   
    |> (function
      | None -> assert(false) 
      | Some x -> x 
    ) 
  in 

  assert (Pb_option.Constant_int 1 =
        test_default "[default = 1 ]"); 
  assert (Pb_option.Constant_int (-1) =
        test_default "[default = -1]"); 
  assert (Pb_option.Constant_string "hello" =
        test_default "[default = \"hello\"]"); 
  assert (Pb_option.Constant_string "he'l'lo" =
        test_default "[default = \"he'l'lo\"]"); 
  assert (Pb_option.Constant_string "he\"l\"lo" =
        test_default "[default = \"he\\\"l\\\"lo\"]"); 
  assert (Pb_option.Constant_float 1.23 =
        test_default "[default = 1.23]"); 
  assert (Pb_option.Constant_float (-. 1.23) =
        test_default "[default = -1.23]"); 
  assert (Pb_option.Constant_bool true =
        test_default "[default = true]"); 
  assert (Pb_option.Constant_bool false =
        test_default "[default = false]"); 
  () 

let () = 
  let field_options = parse Pb_parsing_parser.field_options_ "[]" in 
  assert (field_options = Pb_option.empty)

let () = 
  let field_options = parse Pb_parsing_parser.field_options_ "[a=1,b=true]" in 
  assert(has_option field_options "a"); 
  assert(has_option field_options "b"); 
  ()

let () = 
  let field_options = 
    parse Pb_parsing_parser.field_options_ "[(ocaml_type) = int]" 
  in 
  assert (Some (Pb_option.Constant_litteral "int") = 
            Pb_option.get field_options "ocaml_type")

let () =
  print_endline "Parse Field Options ... Ok"
