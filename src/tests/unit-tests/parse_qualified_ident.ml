let parse f s = f Pb_parsing_lexer.lexer (Lexing.from_string s)

module Pt = Pb_parsing_parse_tree

let field_type s = (parse Pb_parsing_parser.normal_field_ s).Pt.field_type

(* protobuf treats whitespace and comments as insignificant, so each of these
 * spellings names the same type as the canonical "a.b.c.Inner". *)
let () =
  let canonical = field_type "a.b.c.Inner f = 1;" in
  List.iter
    (fun spelling -> assert (field_type spelling = canonical))
    [
      "a.b.c\n      .Inner f = 1;";
      "a.b.c .Inner f = 1;";
      "a.b.c. Inner f = 1;";
      "a . b . c . Inner f = 1;";
      "a.b.c // trailing comment\n      .Inner f = 1;";
      "a.b.c /* inline comment */ .Inner f = 1;";
      "a\n  .b\n  .c\n  .Inner f = 1;";
    ]

(* the path segments and the leading dot (which marks a name resolved from the
 * root scope) both survive being split up *)
let () =
  assert (
    field_type "a . b . C f = 1;"
    = `User_defined
        {
          Pb_field_type.type_path = [ "a"; "b" ];
          type_name = "C";
          from_root = false;
        });
  assert (
    field_type ". a.b.C f = 1;"
    = `User_defined
        {
          Pb_field_type.type_path = [ "a"; "b" ];
          type_name = "C";
          from_root = true;
        })

(* a keyword is a legal path segment, split or not *)
let () =
  assert (field_type "a . map . C f = 1;" = field_type "a.map.C f = 1;");
  assert (field_type "a . to . C f = 1;" = field_type "a.to.C f = 1;");
  assert (field_type "a . returns . C f = 1;" = field_type "a.returns.C f = 1;")

(* builtin types keep resolving to their builtin, and are not turned into a
 * single segment user defined name *)
let () =
  assert (field_type "int32 f = 1;" = `Int32);
  assert (field_type "string f = 1;" = `String)

(* a name may be split in any other position a type is named *)
let () =
  let proto = parse Pb_parsing_parser.proto_ "package a . b . c;" in
  assert (proto.Pt.package = Some "a.b.c")

let oneof_field_type s =
  match (parse Pb_parsing_parser.oneof_ s).Pt.oneof_body with
  | [ Pt.Oneof_field field ] -> field.Pt.field_type
  | [] | Pt.Oneof_option _ :: _ | _ :: _ :: _ -> assert false

(* both oneof field forms, with and without field options *)
let () =
  let expected =
    `User_defined
      { Pb_field_type.type_path = [ "p" ]; type_name = "V"; from_root = false }
  in
  assert (oneof_field_type "oneof o { p\n  . V v = 1; }" = expected);
  assert (
    oneof_field_type "oneof o { p . V v = 1 [deprecated = true]; }" = expected)

(* every labelled and optioned normal_field form names its type the same way *)
let () =
  let expected =
    `User_defined
      { Pb_field_type.type_path = [ "a"; "b" ]; type_name = "C"; from_root = false }
  in
  List.iter
    (fun spelling -> assert (field_type spelling = expected))
    [
      "a . b . C f = 1;";
      "a . b . C f = 1 [deprecated = true];";
      "optional a . b . C f = 1;";
      "repeated a . b . C f = 1 [deprecated = true];";
    ]

(* the unsplit leading dot form reaches qualified_ident through its dot leading
 * token, rather than through a detached dot *)
let () =
  assert (
    field_type ".a.b.C f = 1;"
    = `User_defined
        {
          Pb_field_type.type_path = [ "a"; "b" ];
          type_name = "C";
          from_root = true;
        })

(* a map value type *)
let map_value_type s =
  match (parse Pb_parsing_parser.message_ s).Pt.message_body with
  | [ Pt.Message_map_field field ] -> field.Pt.map_value_type
  | []
  | _ :: _ :: _
  | ( Pt.Message_field _ | Pt.Message_oneof_field _ | Pt.Message_sub _
    | Pt.Message_enum _ | Pt.Message_extension _ | Pt.Message_reserved _
    | Pt.Message_option _ )
    :: _ ->
    assert false

let () =
  assert (
    map_value_type "message M { map<string, p . V> m = 1; }"
    = map_value_type "message M { map<string,p.V> m = 1; }")

(* an rpc request and response type *)
let rpc_types s =
  match (parse Pb_parsing_parser.service_ s).Pt.service_body with
  | [ Pt.Service_rpc rpc ] -> rpc.Pt.rpc_req, rpc.Pt.rpc_res
  | [] | _ :: _ :: _ | Pt.Service_option _ :: _ -> assert false

let () =
  assert (
    rpc_types "service S { rpc Go (a . b . Req) returns (. c.Res); }"
    = rpc_types "service S { rpc Go (a.b.Req) returns (.c.Res); }")

(* an extend target *)
let () =
  let extend =
    parse Pb_parsing_parser.extend_ "extend p . Foo { optional int32 b = 100; }"
  in
  assert (extend.Pt.extend_name = "p.Foo")

(* a parenthesised option extension name, and the dot leading continuation that
 * "option (ext).sub" depends on *)
let () =
  assert (
    parse Pb_parsing_parser.option_ "option (a . b) = 1;"
    = parse Pb_parsing_parser.option_ "option (a.b) = 1;");
  let name, _ = parse Pb_parsing_parser.option_ "option (ext).sub = 1;" in
  assert (
    name
    = [ Pb_raw_option.Extension_name "ext"; Pb_raw_option.Simple_name "sub" ])

(* An empty segment, a trailing dot and a lone dot are all parse errors, and
 * must be reported as such: a lone dot used to be matched by the float literal
 * rule and escaped as Failure "float_of_string" rather than a parse error. *)
let rejected s =
  match parse Pb_parsing_parser.normal_field_ s with
  | _ -> false
  | exception Failure _ -> false
  | exception Parsing.Parse_error -> true

let () =
  assert (rejected "a..b.C f = 1;");
  assert (rejected "a.b.c. = 1;");
  assert (rejected "int32 . = 1;")

let () = print_endline "Parse Qualified Ident ... Ok"
