(*
  The MIT License (MIT)
  
  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)

type t = {
  file_name: string option;
  line: int;
  col: int;
}

let from_lexbuf lexbuf =
  let open Lexing in
  let file_name =
    match lexbuf.lex_curr_p.pos_fname with
    | "" -> None
    | x -> Some x
  in

  let line = lexbuf.lex_curr_p.pos_lnum in
  let col = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in

  { file_name; line; col }

let to_string { file_name; line; col } =
  Printf.sprintf "%s:%i:%i: " (Pb_util.Option.default "" file_name) line col
(* standard compilation error format *)
