
type codegen_f = ?and_:unit -> Ocaml_types.type_ -> Fmt.scope -> bool 

let all_code_gen = [
  (module Codegen_type: Codegen.S);
  (module Codegen_default: Codegen.S);
  (module Codegen_decode: Codegen.S);
  (module Codegen_encode: Codegen.S);
  (module Codegen_pp: Codegen.S);
]

let generate_code otypes proto_file_name = 
  (* -- `.ml` file -- *)

  let gen otypes sc (fs:(codegen_f*string option) list)  = 
    List.iter (fun ((f:codegen_f), ocamldoc_title)-> 
      begin
        match ocamldoc_title with
        | None -> () 
        | Some ocamldoc_title -> ( 
            Fmt.empty_line sc;
            Fmt.line sc @@ Codegen_util.sp "(** {2 %s} *)" ocamldoc_title;  
            Fmt.empty_line sc;
        )
      end;

      List.iter (fun types -> 
        let _:bool = List.fold_left (fun first  type_ -> 
          let has_encoded = if first 
            then f type_ sc 
            else f ~and_:() type_ sc
          in 
          Fmt.empty_line sc;
          first && (not has_encoded) 
        ) true types in 
        ()
      ) otypes 
    ) fs 
  in

  let sc = Fmt.empty_scope () in 
  Fmt.line sc "[@@@ocaml.warning \"-30\"]";
  Fmt.empty_line sc;
  gen otypes  sc (List.map (fun m -> 
    let module C = (val m:Codegen.S) in 
    C.gen_struct, None
  ) all_code_gen);

  let struct_string = (Fmt.print sc) in 

  (* -- `.mli` file -- *)

  let sc = Fmt.empty_scope () in 
  Fmt.line sc @@ 
    Codegen_util.sp "(** %s Generated Types and Encoding *)" (Filename.basename proto_file_name); 
  gen otypes  sc (List.map (fun m -> 
    let module C = (val m:Codegen.S) in 
    C.gen_sig, Some C.ocamldoc_title
  ) all_code_gen);

  let sig_string = Fmt.print sc in
  (sig_string, struct_string)


let compile proto_definition = 
  let lexbuf = Lexing.from_string proto_definition in 
  let proto  = 
    try 
      Pbparser.proto_ Pblexer.lexer lexbuf 
    with exn -> 
      raise (Exception.add_loc (Loc.from_lexbuf lexbuf) exn)
  in  

  let all_pbtt_msgs = Pbtt_util.compile_proto_p1 "tmp.proto" proto in 
  let all_pbtt_msgs = List.map (Pbtt_util.compile_proto_p2 all_pbtt_msgs) all_pbtt_msgs in 
  let grouped_pbtt_msgs = List.rev @@ Pbtt_util.group all_pbtt_msgs in 
  let grouped_ocaml_types = List.map (fun pbtt_msgs -> 
    List.map (fun pbtt_msg -> 
      Backend_ocaml.compile all_pbtt_msgs pbtt_msg 
    ) pbtt_msgs
  ) grouped_pbtt_msgs in 
  let all_ocaml_types = List.flatten grouped_ocaml_types in
  
  generate_code all_ocaml_types "tmp.proto" 

  

