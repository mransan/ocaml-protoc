type t = {
  file_name : string option; 
  line : int; 
} 

let from_lexbuf lexbuf = 
  let file_name = match lexbuf.Lexing.lex_curr_p.Lexing.pos_fname with
    | "" -> None
    | x  -> Some x 
  in 

  let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in 

  {file_name; line} 

let file_name {file_name; _ } = file_name 

let line {line; _ } = line 

let to_string {file_name; line} = 
  Printf.sprintf "File %s, line %i:\n" (Util.option_default "" file_name) line 
