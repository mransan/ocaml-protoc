
let () = Pb_logger.setup_from_out_channel stdout 

let string_of_token = function 
  | Pb_parsing_parser.T_required     ->  "REQUIRED"
  | Pb_parsing_parser.T_optional     ->  "OPTIONAL" 
  | Pb_parsing_parser.T_repeated     ->  "REPEATED" 
  | Pb_parsing_parser.T_message      ->  "MESSAGE"
  | Pb_parsing_parser.T_enum         ->  "ENUM"
  | Pb_parsing_parser.T_package      ->  "PACKAGE"
  | Pb_parsing_parser.T_rbrace       ->  "RBRACE" 
  | Pb_parsing_parser.T_lbrace       ->  "LBRACE" 
  | Pb_parsing_parser.T_rbracket     ->  "RBRACKET"
  | Pb_parsing_parser.T_lbracket     ->  "LBRACKET"
  | Pb_parsing_parser.T_rparen       ->  "RPAREN"
  | Pb_parsing_parser.T_lparen       ->  "LPAREN"
  | Pb_parsing_parser.T_equal        ->  "EQUAL"
  | Pb_parsing_parser.T_semi         ->  "SEMICOLON"
  | Pb_parsing_parser.T_comma        ->  "COMMA"
  | Pb_parsing_parser.T_string s     -> Printf.sprintf "string(%s)" s   
  | Pb_parsing_parser.T_int i        -> Printf.sprintf "int(%i)" i
  | Pb_parsing_parser.T_float f      -> Printf.sprintf "float(%f)" f
  | Pb_parsing_parser.T_ident (_, s) -> Printf.sprintf "ident(%s)" s        
  | Pb_parsing_parser.T_eof          -> "eof" 
  | _ -> assert(false)

let rec loop lexbuf = 
  match Pb_parsing_lexer.lexer lexbuf with 
  | Pb_parsing_parser.T_eof -> () 
  | _ (*as t*)-> 
    (* print_endline @@ string_of_token t; *)
    loop lexbuf
