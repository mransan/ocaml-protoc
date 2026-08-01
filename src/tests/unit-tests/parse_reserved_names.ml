(* Tests for `reserved "name", ...;` support (upstream issue #252).

   Everything here is expressed as a boolean-valued check so that a failure is
   reported by name rather than by an opaque exception. *)

module Pt = Pb_parsing_parse_tree

let parse_reserved s =
  Pb_parsing_parser.reserved_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let parse_message s =
  Pb_parsing_parser.message_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let parse_enum s =
  Pb_parsing_parser.enum_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let parse_oneof s =
  Pb_parsing_parser.oneof_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let reserved_of_body body =
  List.filter_map
    (function
      | Pt.Message_reserved reserved -> Some reserved
      | Pt.Message_field _ | Pt.Message_map_field _ | Pt.Message_oneof_field _
      | Pt.Message_sub _ | Pt.Message_enum _ | Pt.Message_extension _
      | Pt.Message_option _ ->
        None)
    body

let sub_messages_of body =
  List.filter_map
    (function
      | Pt.Message_sub sub -> Some sub
      | Pt.Message_field _ | Pt.Message_map_field _ | Pt.Message_oneof_field _
      | Pt.Message_enum _ | Pt.Message_extension _ | Pt.Message_reserved _
      | Pt.Message_option _ ->
        None)
    body

let sub_enums_of body =
  List.filter_map
    (function
      | Pt.Message_enum sub -> Some sub
      | Pt.Message_field _ | Pt.Message_map_field _ | Pt.Message_oneof_field _
      | Pt.Message_sub _ | Pt.Message_extension _ | Pt.Message_reserved _
      | Pt.Message_option _ ->
        None)
    body

let enum_value_names_of enum =
  List.filter_map
    (function
      | Pt.Enum_value { Pt.enum_value_name; _ } -> Some enum_value_name
      | Pt.Enum_option _ -> None)
    enum.Pt.enum_body

let reserved_of_message s = reserved_of_body (parse_message s).Pt.message_body

(* --- the `reserved_` entry point, name forms ------------------------------ *)

let check_single_name () =
  parse_reserved {|reserved "foo";|} = Pt.Reserved_names [ "foo" ]

let check_multi_name () =
  parse_reserved {|reserved "foo", "bar";|} = Pt.Reserved_names [ "foo"; "bar" ]

(* protoc performs no identifier-shape re-validation on a reserved name, so the
   empty string is accepted and carried through opaquely. *)
let check_empty_name () =
  parse_reserved {|reserved "";|} = Pt.Reserved_names [ "" ]

let check_numeric_still_ranges () =
  parse_reserved "reserved 1, 2;"
  = Pt.Reserved_ranges
      [ Pt.Extension_single_number 1; Pt.Extension_single_number 2 ]

let check_numeric_range_still_ranges () =
  parse_reserved "reserved 9 to 11, 20 to max;"
  = Pt.Reserved_ranges
      [
        Pt.Extension_range (9, Pt.To_number 11);
        Pt.Extension_range (20, Pt.To_max);
      ]

(* --- message-body wiring (pb_parsing_util.message_body_reserved) ---------- *)

let check_message_wiring_numeric () =
  match reserved_of_message "message M { reserved 2, 15; }" with
  | [
   Pt.Reserved_ranges
     [ Pt.Extension_single_number 2; Pt.Extension_single_number 15 ];
  ] ->
    true
  | [] | [ Pt.Reserved_ranges _ ] | [ Pt.Reserved_names _ ] | _ :: _ :: _ ->
    false

let check_message_wiring_names () =
  match reserved_of_message {|message M { reserved "foo", "bar"; }|} with
  | [ Pt.Reserved_names [ "foo"; "bar" ] ] -> true
  | [] | [ Pt.Reserved_names _ ] | [ Pt.Reserved_ranges _ ] | _ :: _ :: _ ->
    false

let check_nested_message_names () =
  match
    sub_messages_of
      (parse_message {|message M { message N { reserved "foo"; } }|})
        .Pt.message_body
  with
  | [ sub ] ->
    reserved_of_body sub.Pt.message_body = [ Pt.Reserved_names [ "foo" ] ]
  | [] | _ :: _ :: _ -> false

(* --- enum bodies ---------------------------------------------------------- *)

(* The grammar deliberately discards enum-level reserved data (enum_values
   drops $1), so the observable property is that the statement parses and the
   surrounding enum values survive intact. *)
let check_enum_names () =
  match parse_enum {|enum E { reserved "FOO", "BAR"; A = 0; }|} with
  | enum -> enum_value_names_of enum = [ "A" ]
  | exception Parsing.Parse_error -> false
  | exception Failure _ -> false

let check_enum_numeric_still_parses () =
  match parse_enum "enum E { reserved 2, 15; A = 0; }" with
  | enum -> enum_value_names_of enum = [ "A" ]
  | exception Parsing.Parse_error -> false
  | exception Failure _ -> false

let check_nested_enum_names () =
  match
    sub_enums_of
      (parse_message {|message M { enum E { reserved "FOO"; A = 0; } }|})
        .Pt.message_body
  with
  | [ sub ] -> enum_value_names_of sub = [ "A" ]
  | [] | _ :: _ :: _ -> false

(* --- the parse-tree printers --------------------------------------------- *)

(* `Pt.pp_proto`'s only consumer in this repo is
   src/tests/expectation/test_option_parsing.ml, and none of its fixture protos
   contains a `reserved` statement, so `pp_reserved` and the `Message_reserved`
   arm of `pp_message_body_content` would otherwise be pinned by nothing.  These
   checks assert the rendered text directly.  Single-element lists are used on
   purpose: the list separator sits inside a vertical box, so a multi-element
   rendering depends on Format's line breaking rather than on the labels under
   test. *)

let render_reserved reserved = Format.asprintf "%a" Pt.pp_reserved reserved

let render_body_content content =
  Format.asprintf "%a" Pt.pp_message_body_content content

let check_pp_names_label () =
  render_reserved (Pt.Reserved_names [ "foo" ]) = {|Reserved_names ["foo"]|}

let check_pp_ranges_label () =
  render_reserved (Pt.Reserved_ranges [ Pt.Extension_single_number 2 ])
  = "Reserved_ranges [(Extension_single_number 2)]"

let check_pp_message_reserved_label () =
  render_body_content (Pt.Message_reserved (Pt.Reserved_names [ "foo" ]))
  = {|Message_reserved Reserved_names ["foo"]|}

(* --- negative controls (protoc rejects all of these) ---------------------- *)

let rejects parse s =
  match parse s with
  | true | false -> false
  | exception Parsing.Parse_error -> true
  | exception Failure _ -> true

let reserved_parses s =
  match parse_reserved s with
  | Pt.Reserved_ranges _ | Pt.Reserved_names _ -> true

let message_parses s =
  match parse_message s with
  | (_ : Pt.message) -> true

let oneof_parses s =
  match parse_oneof s with
  | (_ : Pt.oneof) -> true

let check_bare_ident_rejected () = rejects reserved_parses "reserved foo;"

let check_mixed_number_first_rejected () =
  rejects reserved_parses {|reserved 2, "foo";|}

let check_mixed_name_first_rejected () =
  rejects reserved_parses {|reserved "foo", 2;|}

let check_trailing_comma_rejected () =
  rejects reserved_parses {|reserved "foo", "bar",;|}

let check_empty_statement_rejected () = rejects reserved_parses "reserved;"

let check_message_mixed_rejected () =
  rejects message_parses {|message M { reserved 2, "foo"; }|}

(* protoc's oneof body has no reserved alternative at all ("Expected field
   name."); this fix must not introduce one. *)
let check_oneof_reserved_rejected () =
  rejects oneof_parses {|oneof o { reserved "foo"; }|}

let checks =
  [
    "single quoted name", check_single_name;
    "comma-separated names", check_multi_name;
    "empty name string accepted", check_empty_name;
    "numeric form still Reserved_ranges", check_numeric_still_ranges;
    "numeric range form still Reserved_ranges", check_numeric_range_still_ranges;
    ( "message wiring: numeric -> Message_reserved(Reserved_ranges)",
      check_message_wiring_numeric );
    ( "message wiring: names -> Message_reserved(Reserved_names)",
      check_message_wiring_names );
    "nested message: names retained", check_nested_message_names;
    "enum body: names parse", check_enum_names;
    "enum body: numeric still parses", check_enum_numeric_still_parses;
    "nested enum: names parse", check_nested_enum_names;
    "pp_reserved labels the name case Reserved_names", check_pp_names_label;
    "pp_reserved labels the range case Reserved_ranges", check_pp_ranges_label;
    ( "pp_message_body_content labels the node Message_reserved",
      check_pp_message_reserved_label );
    "bare/unquoted identifier form rejected", check_bare_ident_rejected;
    "mixed number-then-name rejected", check_mixed_number_first_rejected;
    "mixed name-then-number rejected", check_mixed_name_first_rejected;
    "trailing comma rejected", check_trailing_comma_rejected;
    "empty reserved statement rejected", check_empty_statement_rejected;
    "mixed form rejected inside a message body", check_message_mixed_rejected;
    "reserved rejected inside a oneof body", check_oneof_reserved_rejected;
  ]

(* A check that raises (rather than returning false) is a failure too; convert
   the parser's exceptions into the boolean at this one boundary so that every
   failure is reported by name instead of aborting the whole binary. *)
let run_check check =
  match check () with
  | true -> true
  | false -> false
  | exception Parsing.Parse_error -> false
  | exception Failure _ -> false

let () =
  let failed = List.filter (fun (_, check) -> not (run_check check)) checks in
  List.iter (fun (name, _) -> Printf.printf "FAIL: %s\n" name) failed;
  if failed = [] then
    print_endline "Parse Reserved Names... Ok"
  else
    exit 1
