
let () = Logger.setup_from_out_channel stdout 

let string_of_token = function 
  | Parser.REQUIRED     ->  "REQUIRED"
  | Parser.OPTIONAL     ->  "OPTIONAL" 
  | Parser.REPEATED     ->  "REPEATED" 
  | Parser.ONE_OF       ->  "ONE_OF"
  | Parser.MESSAGE      ->  "MESSAGE"
  | Parser.ENUM         ->  "ENUM"
  | Parser.PACKAGE      ->  "PACKAGE"
  | Parser.RBRACE       ->  "RBRACE" 
  | Parser.LBRACE       ->  "LBRACE" 
  | Parser.RBRACKET     ->  "RBRACKET"
  | Parser.LBRACKET     ->  "LBRACKET"
  | Parser.RPAREN       ->  "RPAREN"
  | Parser.LPAREN       ->  "LPAREN"
  | Parser.RANGLEB      ->  "RANGLEB"
  | Parser.LANGLEB      ->  "LANGLEB"
  | Parser.EQUAL        ->  "EQUAL"
  | Parser.SEMICOLON    ->  "SEMICOLON"
  | Parser.COMMA        ->  "COMMA"
  | Parser.STRING s     -> Printf.sprintf "string(%s)" s   
  | Parser.INT i        -> Printf.sprintf "int(%i)" i
  | Parser.FLOAT f      -> Printf.sprintf "float(%f)" f
  | Parser.IDENT s      -> Printf.sprintf "ident(%s)" s        
  | Parser.EOF          -> "eof" 

let rec loop lexbuf = 
  match Lexer.lexer lexbuf with 
  | Parser.EOF -> () 
  | _ as t -> 
    print_endline @@ string_of_token t; 
    loop lexbuf
