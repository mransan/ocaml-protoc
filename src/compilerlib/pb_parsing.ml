module Loc = Pb_location 
module Parsing_util = Pb_parsing_util 
module Pt = Pb_parsing_parse_tree 

let parse_single_file (file_name, file_content) = 
  let lexbuf = Lexing.from_string file_content in 
  let pos    = lexbuf.Lexing.lex_curr_p in 
  lexbuf.Lexing.lex_curr_p <- Lexing.({pos with
    pos_fname = file_name; 
  }); 
  let proto  = 
    try Pb_parsing_parser.proto_ Pb_parsing_lexer.lexer lexbuf 
    with 
    | Parsing.Parse_error -> 
      Pb_exception.ocamlyacc_parsing_error (Loc.from_lexbuf lexbuf) 
    | Pb_exception.Compilation_error e -> 
      Pb_exception.protoc_parsing_error e (Loc.from_lexbuf lexbuf)
    | exn -> 
      let detail = Printexc.to_string exn in 
      Pb_exception.unknown_parsing_error detail (Loc.from_lexbuf lexbuf)
  in  
  let proto = {proto with Pt.proto_file_name = Some file_name} in 
  Parsing_util.verify_syntax_invariants proto;
  proto 

type file_loader = string -> string * string 

let parse_file file_loader file_name = 
  let rec aux protos = function
    | [] -> protos 
    | {Pt.file_name; _} :: imports -> 
      let proto = parse_single_file (file_loader file_name) in 
      let protos = aux (proto::protos) proto.Pt.imports in 
      aux protos imports  
  in 
  let proto = parse_single_file (file_loader file_name) in 
  List.rev @@ aux [proto] proto.Pt.imports  
