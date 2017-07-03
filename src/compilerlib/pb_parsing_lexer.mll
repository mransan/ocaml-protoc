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
{
open Pb_parsing_parser

module Loc = Pb_location

let resolve_identifier loc ident =
  match ident, loc with
  | "message"    , _   -> T_message
  | "required"   , _   -> T_required
  | "optional"   , _   -> T_optional
  | "repeated"   , _   -> T_repeated
  | "oneof"      , loc -> T_one_of loc
  | "enum"       , _   -> T_enum
  | "package"    , _   -> T_package
  | "import"     , loc -> T_import loc
  | "option"     , _   -> T_option
  | "extensions" , _   -> T_extensions
  | "extend"     , _   -> T_extend
  | "syntax"     , _   -> T_syntax
  | "public"     , _   -> T_public
  | "to"         , _   -> T_to
  | "max"        , _   -> T_max
  | "map"        , _   -> T_map
  | "reserved"   , _   -> T_reserved
  | _ , loc -> T_ident (loc, ident)
  (* Note than when updating the list of keywords,
   * the [field_name] rule in pbparser.mly should
   * also be updated to allow field name of the
   * keyword.
   *)

type comment =
  | Comment_value of string
  | Comment_eof

let comment_value s = Comment_value s

let comment_eof     = Comment_eof

type string_ =
  | String_value of string
  | String_eof

let string_value s = String_value s

let string_eof     = String_eof

let update_loc lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- Lexing.({ pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  })

}
let letter        = ['a'-'z' 'A'-'Z']
let identchar     = ['A'-'Z' 'a'-'z' '_' '0'-'9']
let ident         = letter identchar *
let full_ident    = '.' ? ident ("." * ident) *
(* let message_type  = '.' ? (ident '.') ident
 *)
let int_litteral  = ['+' '-']? ['0'-'9']+
let inf_litteral  = ['+' '-']? "inf"

(* TODO fix: somehow E1 for field identified get lexed into a float.
 *)
let float_literal =
  ['+' '-']? ['0'-'9']*
  ('.' ['0'-'9']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9']*)?

let newline  = ('\013'* '\010')
let blank    = [' ' '\009' '\012']

rule lexer = parse
  | "{"         { T_lbrace }
  | "}"         { T_rbrace }
  | "["         { T_lbracket}
  | "]"         { T_rbracket}
  | ")"         { T_rparen }
  | "("         { T_lparen }
  | "<"         { T_less }
  | ">"         { T_greater }
  | "="         { T_equal }
  | ";"         { T_semi }
  | ","         { T_comma }
  | "//"        { match comment [] lexbuf with
    | Comment_eof     -> T_eof
    | Comment_value _ -> lexer lexbuf
  }
  | "/*"        { match multi_line_comment [] lexbuf with
    | Comment_eof     -> T_eof
    | Comment_value _ -> lexer lexbuf
  }
  | "\""          { match string [] lexbuf with
    | String_eof     -> T_eof
    | String_value s -> T_string s
  }
  | int_litteral  { T_int (int_of_string @@ Lexing.lexeme lexbuf) }
  | float_literal { T_float (float_of_string @@ Lexing.lexeme lexbuf) }
  | inf_litteral  { T_float nan }
  | newline       { update_loc lexbuf; lexer lexbuf }
  | blank         { lexer lexbuf }
  | full_ident    { resolve_identifier (Loc.from_lexbuf lexbuf) (Lexing.lexeme lexbuf) }
  | eof           { T_eof }
  | _             { failwith @@ Printf.sprintf "Unknown character found %s" @@
  Lexing.lexeme lexbuf}

and comment l = parse
  | newline {update_loc lexbuf ; comment_value @@ String.concat "" (List.rev l)}
  | _       {comment ((Lexing.lexeme lexbuf)::l) lexbuf }
  | eof     {comment_eof }

and multi_line_comment l = parse
  | newline {update_loc lexbuf ; multi_line_comment l lexbuf }
  | "*/" {
      ignore @@ Lexing.lexeme lexbuf;
      comment_value @@ String.concat "" (List.rev l)
  }
  | _       {multi_line_comment ((Lexing.lexeme lexbuf)::l) lexbuf }
  | eof     { comment_eof }

and string l = parse
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] {
    let c = Lexing.lexeme_char lexbuf 1 in
    string ((Char.escaped c)::l) lexbuf
  }
  | "\""    {string_value  @@ String.concat "" (List.rev l)}
  | _       {string ((Lexing.lexeme lexbuf)::l) lexbuf }
  | eof     {string_eof}
