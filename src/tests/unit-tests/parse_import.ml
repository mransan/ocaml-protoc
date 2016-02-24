
let parse f s  = 
  f Lexer.lexer (Lexing.from_string s)

let () = 
  let x = parse Parser.import_ "import \"blah.proto\";" in
  assert ((Pbpt_util.import "blah.proto") = x)

let () = 
  let x = parse Parser.import_ "import public \"blah.proto\";" in
  assert ((Pbpt_util.import ~public:() "blah.proto") = x)

let () = 
  let x = parse Parser.import_ "import public \"foo/blah.proto\";" in
  assert ((Pbpt_util.import ~public:() "foo/blah.proto") = x)

let () =
  let x =
    match parse Parser.import_ "import ;" with 
    | exception _ -> true
    | _ -> false
  in 
  assert(x)

let () =
  let x =
    match parse Parser.import_ "import boom \"blah.proto\";" with 
    | exception _ -> true
    | _ -> false
  in 
  assert(x)

let () = 
  let proto  = parse Parser.proto_ "import \"blah.proto\";" in
  let imports = proto.Pbpt.imports in  
  assert ((Pbpt_util.import "blah.proto")::[]= imports)

let () = 
  let proto = parse Parser.proto_ "import \"blah.proto\";import public \"boom.proto\";" in
  let imports = proto.Pbpt.imports in 
  assert ((Pbpt_util.import "blah.proto")::
          (Pbpt_util.import ~public:() "boom.proto")::[] = imports)


let () = 
  print_endline "Parse Import ... Ok"
