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

module Loc = Pb_location
module Parsing_util = Pb_parsing_util
module Pt = Pb_parsing_parse_tree

let string_of_token =
  let open Pb_parsing_parser in
  function
  | T_required -> "required"
  | T_optional -> "optional"
  | T_repeated -> "repeated"
  | T_one_of _ -> "oneof"
  | T_message -> "message"
  | T_enum -> "enum"
  | T_package -> "package"
  | T_import _ -> "import"
  | T_public -> "public"
  | T_option -> "option"
  | T_extensions -> "extensions"
  | T_extend -> "extend"
  | T_reserved -> "reserved"
  | T_returns -> "returns"
  | T_rpc -> "rpc"
  | T_service -> "service"
  | T_stream -> "stream"
  | T_syntax -> "syntax"
  | T_to -> "to"
  | T_max -> "max"
  | T_map -> "map"
  | T_rbrace -> "}"
  | T_lbrace -> "{"
  | T_rbracket -> "]"
  | T_lbracket -> "["
  | T_rparen -> ")"
  | T_lparen -> "("
  | T_greater -> ">"
  | T_less -> "<"
  | T_equal -> "="
  | T_semi -> ";"
  | T_colon -> ":"
  | T_comma -> ","
  | T_string s -> Printf.sprintf "%S" s
  | T_int i -> string_of_int i
  | T_float f -> string_of_float f
  | T_ident (_, s) -> s
  | T_eof -> "<EOF>"

(* Custom lexer that buffers tokens *)
let custom_lexer_with_buffer buf_size =
  let token_buffer = Queue.create () in

  let next_token lexbuf =
    let token = Pb_parsing_lexer.lexer lexbuf in
    Queue.add token token_buffer;
    if Queue.length token_buffer > buf_size then
      Queue.take token_buffer |> ignore;
    token
  in

  let error_context_tokens () =
    let context =
      Queue.fold (fun acc tok -> tok :: acc) [] token_buffer
      |> List.rev |> List.map string_of_token |> String.concat " "
    in
    Printf.sprintf "%s <<< HERE" context
  in

  next_token, error_context_tokens

let parse_single_file (file_name, file_content) =
  let lexbuf = Lexing.from_string file_content in
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- Lexing.{ pos with pos_fname = file_name };
  let buffer_size = 5 (* Adjust as needed *) in
  let next_token, error_context_tokens = custom_lexer_with_buffer buffer_size in
  let proto =
    try Pb_parsing_parser.proto_ next_token lexbuf with
    | Parsing.Parse_error ->
      Pb_exception.ocamlyacc_parsing_error (Loc.from_lexbuf lexbuf)
        (error_context_tokens ())
    | Pb_exception.Compilation_error e ->
      Pb_exception.protoc_parsing_error e (Loc.from_lexbuf lexbuf)
        (error_context_tokens ())
    | exn ->
      let msg = Printexc.to_string exn in
      Pb_exception.unknown_parsing_error ~msg ~context:(error_context_tokens ())
        (Loc.from_lexbuf lexbuf)
  in
  let proto = { proto with Pt.proto_file_name = Some file_name } in
  Parsing_util.finalize_proto_value proto

type file_loader = string -> string * string

let parse_file file_loader file_name =
  let rec aux protos = function
    | [] -> protos
    | { Pt.file_name; _ } :: imports ->
      let proto = parse_single_file (file_loader file_name) in
      let protos = aux (proto :: protos) proto.Pt.imports in
      aux protos imports
  in
  let proto = parse_single_file (file_loader file_name) in
  List.rev @@ aux [ proto ] proto.Pt.imports
