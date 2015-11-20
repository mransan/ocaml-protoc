
let parse f s  = 
  f Lexer.lexer (Lexing.from_string s)

let () = 
  let x = parse Parser.import_ "import \"blah.proto\";" in
  assert ((Pbpt_util.import "blah.proto") = x)

let () = 
  let x = parse Parser.import_ "import public \"blah.proto\";" in
  assert ((Pbpt_util.import ~public:() "blah.proto") = x)

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
  let x = parse Parser.import_list_ "import \"blah.proto\";" in
  assert ((Pbpt_util.import "blah.proto")::[]= x)

let () = 
  let x = parse Parser.import_list_ "import \"blah.proto\";import public \"boom.proto\";" in
  assert ((Pbpt_util.import "blah.proto")::
          (Pbpt_util.import ~public:() "boom.proto")::[] = x)


let () = 
  print_endline "Parse Import ... Ok"
