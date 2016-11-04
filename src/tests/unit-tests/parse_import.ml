
let parse f s  = 
  f Pb_parsing_lexer.lexer (Lexing.from_string s)

module Pt_util = Pb_parsing_util 
module Pt = Pb_parsing_parse_tree

let () = 
  let x = parse Pb_parsing_parser.import_ "import \"blah.proto\";" in
  assert ((Pt_util.import "blah.proto") = x)

let () = 
  let x = parse Pb_parsing_parser.import_ "import public \"blah.proto\";" in
  assert ((Pt_util.import ~public:() "blah.proto") = x)

let () = 
  let x = parse Pb_parsing_parser.import_ "import public \"foo/blah.proto\";" in
  assert ((Pt_util.import ~public:() "foo/blah.proto") = x)

let () =
  let x =
    match parse Pb_parsing_parser.import_ "import ;" with 
    | exception _ -> true
    | _ -> false
  in 
  assert(x)

let () =
  let x =
    match parse Pb_parsing_parser.import_ "import boom \"blah.proto\";" with 
    | exception _ -> true
    | _ -> false
  in 
  assert(x)

let () = 
  let proto  = parse Pb_parsing_parser.proto_ "import \"blah.proto\";" in
  let imports = proto.Pt.imports in  
  assert ((Pt_util.import "blah.proto")::[]= imports)

let () = 
  let proto = parse Pb_parsing_parser.proto_ "import \"blah.proto\";import public \"boom.proto\";" in
  let imports = proto.Pt.imports in 
  assert ((Pt_util.import "blah.proto")::
          (Pt_util.import ~public:() "boom.proto")::[] = imports)


let () = 
  print_endline "Parse Import ... Ok"
