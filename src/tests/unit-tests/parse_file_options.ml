let parse s =
  Pb_parsing_parser.option_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let parse_proto s =
  Pb_parsing_parser.proto_ Pb_parsing_lexer.lexer (Lexing.from_string s)

module Pt = Pb_parsing_parse_tree

let () =
  let s = "option blah = 1;" in
  let fo =
    [ Pb_option.Simple_name "blah" ], Pb_option.(Scalar_value (Constant_int 1))
  in
  assert (fo = parse s)

let () =
  let s = "option blah = 1; ;  option foo = \"blah\"; " in
  let proto = parse_proto s in
  let fo_parsed = proto.Pt.file_options in
  assert (
    Some Pb_option.(Scalar_value (Constant_int 1))
    = Pb_option.get fo_parsed [ Pb_option.Simple_name "blah" ]);
  assert (
    Some Pb_option.(Scalar_value (Constant_string "blah"))
    = Pb_option.get fo_parsed [ Pb_option.Simple_name "foo" ]);
  ()

let () = print_endline "Parse File Options ... Ok"
