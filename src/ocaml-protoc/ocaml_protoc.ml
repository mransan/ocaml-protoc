(*
  The MIT License (MIT)
  
  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)

module L = Logger 
module E = Exception
  
let caml_file_name_of_proto_file_name = Codegen_util.caml_file_name_of_proto_file_name 

let imported_filename include_dirs file_name = 
  if Sys.file_exists file_name
  then Some file_name
  else 
    List.fold_left (fun found_file include_dir -> 
      let try_file_name = Filename.concat include_dir file_name in 
      match found_file, Sys.file_exists try_file_name with 
      | None         , true  -> Some try_file_name 
      | Some previous, true  -> (
        Printf.eprintf "[Warning] Imported file %s found in 2 directories, picking: %s\n"
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
  let usage = "ocaml-protoc -ml_out <output_directory> <file_name>.proto" in
  Arg.parse cmd_line_args anon_fun usage;
  (if !proto_file_name = ""
   then failwith "Missing protobuf file name from command line argument"); 
  (if !ml_out = ""
   then failwith "Missing -ml_out (output directory) from command line argument"); 

  let out_file_name = 
    let basename = Filename.basename !proto_file_name in 
    let caml_basename = caml_file_name_of_proto_file_name basename in  
    Filename.concat !ml_out caml_basename 
  in

  let generated_files = [] in
     
  let struct_oc, generated_files = match out_file_name with 
    | "" -> stdout, generated_files  
    | _  -> 
      let ml_file_name = out_file_name ^ ".ml" in 
      open_out ml_file_name, ml_file_name :: generated_files 
  in
  let sig_oc, generated_files = match out_file_name with 
    | "" -> stdout, generated_files 
    | _  -> 
      let ml_file_name = out_file_name ^ ".mli" in 
      open_out ml_file_name, ml_file_name :: generated_files 
  in

  (!proto_file_name, !include_dirs, sig_oc, struct_oc, !debug, generated_files)  


(* -- main -- *)

let compile include_dirs proto_file_name = 

  let rec loop acc = function
    | None -> acc 
    | Some file_name -> 

      let pbtt_msgs, files_options = acc in 

      let file_name = match imported_filename include_dirs file_name with
        | Some file_name -> file_name 
        | None -> raise @@ E.import_file_not_found file_name 
      in  
      
      let ic     = open_in file_name in 
      let lexbuf = Lexing.from_channel ic in 
      let pos    = lexbuf.Lexing.lex_curr_p in 
      lexbuf.Lexing.lex_curr_p <- Lexing.({pos with
        pos_fname = file_name; 
      }); 
      let proto  = 
        try 
          Pbparser.proto_ Pblexer.lexer lexbuf 
        with exn -> 
          raise (Exception.add_loc (Loc.from_lexbuf lexbuf) exn)
      in  
      close_in ic; 
      let files_options = (file_name, proto.Pbpt.file_options) :: files_options in 
      let pbtt_msgs = pbtt_msgs @ Pbtt_util.compile_proto_p1 file_name proto in 

      let acc = (pbtt_msgs, files_options) in 
      List.fold_left (fun acc {Pbpt.file_name; _ } -> 
        loop acc (Some file_name) 
      ) acc proto.Pbpt.imports 
  in 

  let pbtt_msgs, files_options = loop ([], []) (Some proto_file_name) in  

  List.iter (function 
    | {Pbtt.spec = Pbtt.Message  msg; id; scope; _  }   -> L.endline @@ Pbtt_util.string_of_message id scope msg
    | {Pbtt.spec = Pbtt.Enum {Pbtt.enum_name; _ }; _  } -> L.endline @@ enum_name 
  ) pbtt_msgs; 

  let pbtt_msgs = List.map (Pbtt_util.compile_proto_p2 pbtt_msgs) pbtt_msgs in 

  let grouped_proto = List.rev @@ Pbtt_util.group pbtt_msgs in 

  (*
   * Only get the types which are part of the given protofile (compilation unit)
   *)

  let grouped_proto = List.filter (function
    | {Pbtt.file_name; _ }::_ when file_name = proto_file_name -> true 
    | _ -> false
  ) grouped_proto in 

  let proto_file_options = List.assoc proto_file_name files_options in  
  
  (* -- OCaml Backend -- *)

  let module BO = Backend_ocaml in 

  let otypes = List.rev @@ List.fold_left (fun otypes types -> 
    let l = List.flatten @@ List.map (fun t -> BO.compile pbtt_msgs t) types in 
    l :: otypes
  ) [] grouped_proto  in 

  (otypes, proto_file_options)

type codegen_f = ?and_:unit -> Ocaml_types.type_ -> Fmt.scope -> bool 


let all_code_gen = [
  (module Codegen_type: Codegen.S);
  (module Codegen_default: Codegen.S);
  (module Codegen_decode: Codegen.S);
  (module Codegen_encode: Codegen.S);
  (module Codegen_pp: Codegen.S);
]
  

let generate_code sig_oc struct_oc otypes proto_file_options proto_file_name = 

  (* 
   * File level ppx extension (ie @@@ type of ppx) 
   *)

  let print_ppx sc = 
    match Pbpt_util.file_option proto_file_options "ocaml_file_ppx" with
    | None -> () 
    | Some Pbpt.Constant_string s -> Fmt.line sc @@ Printf.sprintf "[@@@%s]" s
    | _ -> E.invalid_ppx_extension_option proto_file_name  
  in 

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
  
  (* -- `.ml` file -- *)

  let sc = Fmt.empty_scope () in 
  Fmt.line sc "[@@@ocaml.warning \"-27-30-39\"]";
  print_ppx sc; 
  Fmt.empty_line sc;
  gen otypes  sc (List.map (fun m -> 
    let module C = (val m:Codegen.S) in 
    C.gen_struct, None
  ) all_code_gen);

  output_string struct_oc (Fmt.print sc);

  (* -- `.mli` file -- *)

  let sc = Fmt.empty_scope () in 
  Fmt.line sc @@ 
    Codegen_util.sp "(** %s Generated Types and Encoding *)" (Filename.basename proto_file_name); 
  Fmt.empty_line sc; 
  print_ppx sc; 
  gen otypes  sc (List.map (fun m -> 
    let module C = (val m:Codegen.S) in 
    C.gen_sig, Some C.ocamldoc_title
  ) all_code_gen);

  output_string sig_oc (Fmt.print sc);
  ()

let () = 

  let (
    proto_file_name, 
    include_dirs, 
    sig_oc, 
    struct_oc, 
    enable_debugging, 
    generated_files
  ) = parse_args () in 

  if enable_debugging
  then L.setup_from_out_channel stdout;

  let close_file_channels () = 
    close_out struct_oc; 
    close_out sig_oc
  in 

  try
    let otypes, proto_file_options = compile include_dirs proto_file_name in 
    generate_code sig_oc struct_oc otypes proto_file_options proto_file_name;
    close_file_channels ();
    List.iter (fun file_name ->
      Printf.printf "Generated %s\n" file_name; 
    ) generated_files; 
    ()
  with exn -> 
    close_file_channels ();
    List.iter (fun file_name ->
      Sys.remove file_name
    ) generated_files; 
    (raise exn : unit)
