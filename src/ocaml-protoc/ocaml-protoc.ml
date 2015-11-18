
module L = Logger 
  
let caml_file_name_of_proto_file_name = Ocaml_codegen.caml_file_name_of_proto_file_name 

(** [parse_args ()] parses the command line argument 
    and returns [(in_channel, out_channel)] where 
    in_channel is where the protobuf definition can be read from 
    and out_channel where the generated code should be outputed.
  *)
let parse_args () = 
  let proto_file_name = ref "" in  
  let debug           = ref false in  
  let cmd_line_args = [
    ("-debug", Arg.Set debug, "enable debugging");  
  ] in 
  let anon_fun  = (fun proto_file -> 
    proto_file_name := proto_file
  )  in 
  let usage = "protobufo.tsk -out <file_name> <file_name>.proto" in  
  Arg.parse cmd_line_args anon_fun usage;
  assert(!proto_file_name <> ""); 

  let out_file_name  = caml_file_name_of_proto_file_name !proto_file_name in  
  Printf.printf "proto: %s -> caml: %s \n" !proto_file_name out_file_name; 
  let struct_oc = match out_file_name with 
    | "" -> stdout 
    | _  -> open_out (out_file_name ^ ".ml") in 
  let sig_oc = match out_file_name with 
    | "" -> stdout 
    | _  -> open_out (out_file_name ^ ".mli") in 
  (open_in !proto_file_name, sig_oc, struct_oc, !debug)  

let () = 

  let ic, sig_oc, struct_oc, enable_debugging = parse_args () in 

  if enable_debugging
  then L.setup_from_out_channel stdout;
    
  (* -- Compilation -- *)
  let proto = 
    Parser.proto_ Lexer.lexer (Lexing.from_channel ic)
  in 
  let scope     = Pbtt_util.scope_of_package proto.Pbpt.package in 
  let astc_msgs = List.fold_left (fun astc_msgs ast_msg -> 
    astc_msgs @ Pbtt_util.compile_message_p1 scope ast_msg
  ) [] proto.Pbpt.messages in 
  L.log "-- Phase 1 --\n"; 
  List.iter (function 
    | Pbtt.Message  msg -> L.endline @@ Pbtt_util.string_of_message msg
    | Pbtt.Enum {Pbtt.enum_name; _ } -> L.endline @@ enum_name 
  ) astc_msgs; 
  let astc_msgs = List.map (Pbtt_util.compile_type_p2 astc_msgs) astc_msgs in 

  let wrap s = 
    if s <> "" then [s ; "\n\n" ] else [s] 
  in 

  (* -- OCaml Backend -- *)

  let grouped_proto = List.rev @@ Pbtt_util.group astc_msgs in 

  let module BO = Backend_ocaml in 
  let otypes = List.rev @@ List.fold_left (fun otypes types -> 
    let l = List.flatten @@ List.map (fun t -> BO.compile astc_msgs t) types in 
    l :: otypes
  ) [] grouped_proto  in 


  (* -- `.ml` file -- *)

  let gen types (f:(?and_:unit -> Ocaml_types.type_ -> string))  = 
    List.flatten @@ List.rev @@ fst (List.fold_left (fun (sl, first) type_ -> 
    (if first 
    then wrap @@ f type_ 
    else wrap @@ f ~and_:() type_)::sl, false 
  ) ([], true) types) 
  in 

  let gen_opt types (f:(?and_:unit -> Ocaml_types.type_ -> string option))  = 
    List.flatten @@ List.rev @@ fst (List.fold_left (fun (sl, first) type_ -> 
      let s = 
        if first 
        then f type_ 
        else f ~and_:() type_ in 
      match s with 
      | Some s -> (wrap s)::sl, false 
      | None   -> sl   , first 
  ) ([], true) types) 
  in 

  let concat = Util.concat in 
  
  output_string struct_oc @@ concat [
    Backend_ocaml_static.prefix_payload_to_ocaml_t;
    Backend_ocaml_static.prefix_decode_f;
    "\n";
    concat @@ List.map (fun types_ -> 
      concat @@ List.flatten [
      gen     types_ Ocaml_codegen.gen_type ;
      gen_opt types_ Ocaml_codegen.gen_decode;
      gen_opt types_ Ocaml_codegen.gen_encode;
      gen_opt types_ Ocaml_codegen.gen_string_of;
      ]
    ) otypes;
  ];

  (* -- `.mli` file -- *)

  let wrap_opt = function | Some x -> wrap x | None -> [] in 

  output_string sig_oc @@ concat @@ List.map (fun types_ -> 
    concat @@ List.flatten [
      gen      types_ Ocaml_codegen.gen_type;
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_decode_sig t) types_ ;
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_encode_sig t) types_ ;
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_string_of_sig t) types_ ;
    ]
  ) (otypes)
