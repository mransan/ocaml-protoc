
module L = Logger 
module E = Exception
  
let caml_file_name_of_proto_file_name = Ocaml_codegen.caml_file_name_of_proto_file_name 

let imported_filename include_dirs file_name = 
  if Sys.file_exists file_name
  then Some file_name
  else 
    List.fold_left (fun found_file include_dir -> 
      let try_file_name = Filename.concat include_dir file_name in 
      match found_file, Sys.file_exists try_file_name with 
      | None         , true  -> Some try_file_name 
      | Some previous, true  -> (
        Printf.printf "Imported file %s found in 2 directories, picking: %s\n"
          file_name previous; 
        found_file
      )
      | _, false -> found_file  
    ) None include_dirs


(** [parse_args ()] parses the command line argument 
    and returns [(in_channel, out_channel)] where 
    in_channel is where the protobuf definition can be read from 
    and out_channel where the generated code should be outputed.
  *)
let parse_args () = 
  let proto_file_name = ref "" in  
  let debug           = ref false in  
  let include_dirs    = ref [] in 
  let include_dirs_spec = (fun dir -> 
    include_dirs := dir :: (!include_dirs)
  ) in 
  let ml_out          = ref "" in 
   
  let cmd_line_args = [
    ("-debug"  , Arg.Set debug               , "enable debugging");  
    ("-I"      , Arg.String include_dirs_spec, "include directories");  
    ("-ml_out" , Arg.Set_string  ml_out      , "output directory");  
  ] in 
  let anon_fun  = (fun proto_file -> 
    proto_file_name := proto_file
  )  in 
  let usage = "protobufo.tsk -out <file_name> <file_name>.proto" in  
  Arg.parse cmd_line_args anon_fun usage;
  assert(!proto_file_name <> ""); 
  assert(!ml_out          <> ""); 

  let out_file_name = 
    let basename = Filename.basename !proto_file_name in 
    let caml_basename = caml_file_name_of_proto_file_name basename in  
    Filename.concat !ml_out caml_basename 
  in
     
  Printf.printf "proto: %s -> caml: %s \n" !proto_file_name out_file_name; 
  let struct_oc = match out_file_name with 
    | "" -> stdout 
    | _  -> open_out (out_file_name ^ ".ml") in 
  let sig_oc = match out_file_name with 
    | "" -> stdout 
    | _  -> open_out (out_file_name ^ ".mli") in 
  (!proto_file_name, !include_dirs, sig_oc, struct_oc, !debug)  


(* -- main -- *)

let () = 

  let proto_file_name, include_dirs, sig_oc, struct_oc, enable_debugging = parse_args () in 

  Printf.printf "include dirs %s \n" @@ String.concat ", " include_dirs;  

  if enable_debugging
  then L.setup_from_out_channel stdout;

  let rec loop acc = function
    | None -> acc 
    | Some file_name -> 

      let file_name = match imported_filename include_dirs file_name with
        | Some file_name -> file_name 
        | None -> raise @@ E.import_file_not_found file_name 
      in  
      
      let ic    = open_in file_name in 
      let proto = Parser.proto_ Lexer.lexer (Lexing.from_channel ic) in 
      close_in ic; 
      let pbtt_msgs = acc @ Pbtt_util.compile_proto_p1 file_name proto in 
      let pbtt_msgs = List.fold_left (fun pbtt_msgs {Pbpt.file_name; _ } -> 
        loop pbtt_msgs (Some file_name) 
      ) pbtt_msgs proto.Pbpt.imports in 
      pbtt_msgs 
  in 

  let pbtt_msgs = loop [] (Some proto_file_name) in  

  List.iter (function 
    | {Pbtt.spec = Pbtt.Message  msg; id; scope; _  }   -> L.endline @@ Pbtt_util.string_of_message id scope msg
    | {Pbtt.spec = Pbtt.Enum {Pbtt.enum_name; _ }; _  } -> L.endline @@ enum_name 
  ) pbtt_msgs; 

  let pbtt_msgs = List.map (Pbtt_util.compile_proto_p2 pbtt_msgs) pbtt_msgs in 


  (* -- OCaml Backend -- *)

  let grouped_proto = List.rev @@ Pbtt_util.group pbtt_msgs in 

  let grouped_proto = List.filter (function
    | {Pbtt.file_name; _ }::_ when file_name = proto_file_name -> true 
    | _ -> false
  ) grouped_proto in 

  let module BO = Backend_ocaml in 
  let otypes = List.rev @@ List.fold_left (fun otypes types -> 
    let l = List.flatten @@ List.map (fun t -> BO.compile pbtt_msgs t) types in 
    l :: otypes
  ) [] grouped_proto  in 


  let wrap s = 
    if s <> "" then [s ; "\n\n" ] else [s] 
  in 
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
    "\n";
    concat @@ List.map (fun types_ -> 
      concat @@ List.flatten [
      gen     types_ Ocaml_codegen.gen_type ;
      gen_opt types_ Ocaml_codegen.gen_decode;
      gen_opt types_ Ocaml_codegen.gen_encode;
      gen_opt types_ Ocaml_codegen.gen_string_of;
      gen_opt types_ Ocaml_codegen.gen_default;
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
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_default_sig t) types_ ;
    ]
  ) (otypes)
