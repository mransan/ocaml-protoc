module Pbpt = Pb_parsing_parse_tree

let parse s =
  Pb_parsing_parser.extend_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let () =
  let s = "extend M {} " in
  let ev = parse s in
  assert ("M" = ev.Pbpt.extend_name);
  assert ([] = ev.Pbpt.extend_body);
  ()

let () =
  let s = "extend M {optional int32 i = 1; ; }; " in
  let ev = parse s in
  assert ("M" = ev.Pbpt.extend_name);
  (match ev.Pbpt.extend_body with
  | { Pbpt.field_name; field_number; field_label; field_type; field_options }
    :: [] ->
    assert (field_name = "i");
    assert (field_number = 1);
    assert (field_type = `Int32);
    assert (field_options = Pb_raw_option.empty);
    assert (`Optional = field_label);
    ()
  | _ -> (assert false : unit));
  ()

let () =
  let s =
    "extend M {\n    optional int32 i = 1;\n    required double j = 2; \n  } "
  in
  let ev = parse s in
  assert ("M" = ev.Pbpt.extend_name);
  (match ev.Pbpt.extend_body with
  | [
   { Pbpt.field_name; field_number; field_label; field_type; field_options }; f2;
  ] ->
    assert (field_name = "i");
    assert (field_number = 1);
    assert (field_type = `Int32);
    assert (field_options = Pb_raw_option.empty);
    assert (`Optional = field_label);
    let {
      Pbpt.field_name;
      field_number;
      field_label;
      field_type;
      field_options;
    } =
      f2
    in
    assert (field_name = "j");
    assert (field_number = 2);
    assert (field_type = `Double);
    assert (field_options = Pb_raw_option.empty);
    assert (`Required = field_label);
    ()
  | _ -> (assert false : unit));
  ()

let () = print_endline "Parse Extend ... Ok"
