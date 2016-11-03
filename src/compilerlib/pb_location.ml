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

let to_string {file_name; line} = 
  Printf.sprintf "%s:%i:0: " (Pb_util.Option.default "" file_name) line 
(* standard compilation error format *)
