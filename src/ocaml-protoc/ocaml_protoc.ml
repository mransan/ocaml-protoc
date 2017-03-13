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

module L = Pb_logger 
module E = Pb_exception
module Loc = Pb_location
module Pt = Pb_parsing_parse_tree 
module Parsing_util = Pb_parsing_util
module Tt = Pb_typing_type_tree 
module Typing_util = Pb_typing_util
module Ot = Pb_codegen_ocaml_type 
module F = Pb_codegen_formatting

(* [ocaml-protoc] provides the ability to override all the custom 
 *
 * protobuf file options defined in 
 * src/include/ocaml-protoc/ocamloptions.proto as command line arguments. 
 *
 * This module implements the bridge functionality between the 2 
 * mechanism; command line options are converted to file options and 
 * appended there. 
 *)
module File_options = struct

  type t = {
    mutable int32_type : string option;
    mutable int64_type : string option; 
    mutable ocaml_file_ppx : string option; 
    mutable ocaml_all_types_ppx : string option;  
  } 
  (* all file options supported... this needs to be kept in sync with
   * src/include/ocaml-protoc/ocamloptions.proto *)

  let make () = {
    int32_type = None; 
    int64_type = None; 
    ocaml_file_ppx = None; 
    ocaml_all_types_ppx = None; 
  }  

  (* Compute the command line arguments for be used with the Arg module.  *)
  let cmd_line_args t = [
    (
      "-int32_type", 
      Arg.String (function  
        | "int_t" -> t.int32_type <- Some "int_t" 
        | x -> failwith @@ Printf.sprintf "Invalid int32_type value %s" x
      ),
      "int32_type file option"
    );
    (
      "-int64_type", 
      Arg.String (function  
        | "int_t" -> t.int64_type <- Some "int_t" 
        | x -> failwith @@ Printf.sprintf "Invalid int64_type value %s" x
      ),
      "int64_type file option"
    );
    (
      "-ocaml_file_ppx",
      Arg.String (fun s -> t.ocaml_file_ppx <- Some s), 
      "ocaml_file_ppx file option"
    );
    (
      "-ocaml_all_types_ppx",
      Arg.String (fun s -> t.ocaml_all_types_ppx<- Some s), 
      "ocaml_all_types_ppx file option"
    );
  ]  

  (** Converts the command line values to Parse Tree file options
    *) 
  let to_file_options t : Pb_option.set = 

    let {int32_type; int64_type; ocaml_file_ppx; ocaml_all_types_ppx} = t in 

    let map x f options  = 
      match x with 
      | None -> options
      | Some x -> 
        let option_name, option_value = f x in 
        Pb_option.add options option_name option_value
    in 
    Pb_option.empty
    |> map int32_type (fun s -> 
      ("int32_type", Pb_option.Constant_litteral s)
    )
    |> map int64_type (fun s -> 
      ("int64_type", Pb_option.Constant_litteral s)
    )
    |> map ocaml_file_ppx (fun s -> 
      ("ocaml_file_ppx", Pb_option.Constant_string s)
    )
    |> map ocaml_all_types_ppx (fun s -> 
      ("ocaml_all_types_ppx", Pb_option.Constant_string s)
    )

end 
  
let ocaml_file_name_of_proto_file_name = 
  Pb_codegen_util.caml_file_name_of_proto_file_name 

let imported_filename include_dirs file_name = 
  if Sys.file_exists file_name
  then file_name
  else 
    let found_file = List.fold_left (fun found_file include_dir -> 
      let try_file_name = Filename.concat include_dir file_name in 
      match found_file, Sys.file_exists try_file_name with 
      | None , true  -> Some try_file_name 
      | Some previous, true  -> (
        Printf.eprintf 
            ("[Warning] Imported file %s found in 2 directories, " ^^ 
             "picking: %s\n")
            file_name previous; 
        found_file
      )
      | _, false -> found_file  
    ) None include_dirs in

    match found_file with
    | None -> E.import_file_not_found file_name 
    | Some file_name -> file_name 


(* [parse_args ()] parses the command line argument 
 * and returns [(in_channel, out_channel)] where 
 * in_channel is where the protobuf definition can be read from 
 * and out_channel where the generated code should be outputed.  *)
let parse_args () = 
  let proto_file_name = ref "" in  
  let debug = ref false in  
  let include_dirs = ref [] in 
  let include_dirs_spec = (fun dir -> 
    include_dirs := dir :: (!include_dirs)
  ) in 
  let ml_out = ref "" in 
  let cmd_line_files_options = File_options.make () in 
   
  let cmd_line_args = [
    ("-debug", Arg.Set debug, "enable debugging");  
    ("-I", Arg.String include_dirs_spec, "include directories");  
    ("-ml_out", Arg.Set_string  ml_out, "output directory");  
  ] @ File_options.cmd_line_args cmd_line_files_options in 

  let anon_fun  = (fun proto_file -> 
    proto_file_name := proto_file
  )  in 

  let usage = "ocaml-protoc -ml_out <output_directory> <file_name>.proto" in

  Arg.parse cmd_line_args anon_fun usage;

  (* check mandatory arguments are properly set *)
  begin 
    if !proto_file_name = ""
    then failwith "Missing protobuf file name from command line argument"
  end;

  begin 
    if !ml_out = ""
    then failwith
      "Missing -ml_out (output directory) from command line argument"; 
  end;

  (* TODO add a check for the directoy ml_out, does it exists if not the
   * follow the behavior of the Google protoc executable *)

  let out_file_name = 
    let basename = Filename.basename !proto_file_name in 
    let ocaml_basename = ocaml_file_name_of_proto_file_name basename in  
    Filename.concat !ml_out ocaml_basename 
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

  (
    !proto_file_name, 
    !include_dirs, 
    sig_oc, 
    struct_oc, 
    !debug, 
    generated_files, 
    cmd_line_files_options
  )  

let compile cmd_line_files_options include_dirs proto_file_name = 

  (* parsing *) 

  let protos = Pb_parsing.parse_file (fun file_name -> 
    let file_name = imported_filename include_dirs file_name in 
    (file_name, Pb_util.read_file file_name)  
  ) proto_file_name in 

  (* file options can be overriden/added with command line arguments *)
  let protos = List.map (fun proto -> 
    {proto with 
      Pt.file_options = 
        Pb_option.merge proto.Pt.file_options cmd_line_files_options 
    }
  ) protos in

  let proto_file_options = 
    let main_proto = List.hd protos in 
    main_proto.Pt.file_options
  in 

  (* typing *) 

  let grouped_protos = Pb_typing.perform_typing protos in 
  let all_typed_protos = List.flatten grouped_protos in 

  (* Only get the types which are part of the given proto file 
   * (compilation unit) *)
  let grouped_proto = List.filter (function
    | {Tt.file_name; _ }::_ when file_name = proto_file_name -> true 
    | _ -> false
  ) grouped_protos in 

  (* -- OCaml Backend -- *)

  let module BO = Pb_codegen_backend in 

  let ocaml_types = List.rev @@ List.fold_left (fun ocaml_types types -> 
    let l = List.flatten @@ List.map (fun t -> 
      BO.compile all_typed_protos t
    ) types in 
    l :: ocaml_types 
  ) [] grouped_proto  in 

  (ocaml_types, proto_file_options)

type codegen_f = 
  ?and_:unit -> 
  Ot.type_ -> 
  F.scope -> bool 

let all_code_gen = [
  (module Pb_codegen_type_code: Pb_codegen_sig.S);
  (module Pb_codegen_default_code: Pb_codegen_sig.S);
  (module Pb_codegen_decode_code: Pb_codegen_sig.S);
  (module Pb_codegen_encode_code: Pb_codegen_sig.S);
  (module Pb_codegen_pp_code: Pb_codegen_sig.S);
]

let generate_code sig_oc struct_oc otypes proto_file_options proto_file_name = 

  (* File level ppx extension (ie @@@ type of ppx) *)

  let print_ppx sc = 
    match Pb_option.get proto_file_options "ocaml_file_ppx" with
    | None -> () 
    | Some Pb_option.Constant_string s -> 
      F.line sc @@ Printf.sprintf "[@@@%s]" s
    | _ -> E.invalid_ppx_extension_option proto_file_name  
  in 

  let gen otypes sc (fs:(codegen_f*string option) list)  = 
    List.iter (fun ((f:codegen_f), ocamldoc_title)-> 
      begin
        match ocamldoc_title with
        | None -> () 
        | Some ocamldoc_title -> ( 
          F.empty_line sc;
          F.line sc @@ Pb_codegen_util.sp 
              "(** {2 %s} *)" ocamldoc_title;  
          F.empty_line sc;
        )
      end;

      List.iter (fun types -> 
        let _:bool = List.fold_left (fun first  type_ -> 
          let has_encoded = if first 
            then f type_ sc 
            else f ~and_:() type_ sc
          in 
          F.empty_line sc;
          first && (not has_encoded) 
        ) true types in 
        ()
      ) otypes 
    ) fs 
  in
  
  (* -- `.ml` file -- *)

  let sc = F.empty_scope () in 
  F.line sc "[@@@ocaml.warning \"-27-30-39\"]";
  print_ppx sc; 
  F.empty_line sc;
  gen otypes  sc (List.map (fun m -> 
    let module C = (val m:Pb_codegen_sig.S) in 
    C.gen_struct, None
  ) all_code_gen);

  F.line sc "module Make_decoder(Decoder:Pbrt_js.Decoder_sig) = struct";
  F.scope sc (fun sc -> 
    gen otypes sc [ (Pb_codegen_decode_json.gen_struct, None);] 
  ); 
  F.line sc "end";
  
  F.line sc "module Make_encoder(Encoder:Pbrt_js.Encoder_sig) = struct";
  F.scope sc (fun sc -> 
    gen otypes sc [ (Pb_codegen_encode_json.gen_struct, None);]
  ); 
  F.line sc "end";
  

  output_string struct_oc (F.print sc);

  (* -- `.mli` file -- *)

  let sc = F.empty_scope () in 
  F.line sc @@ 
    Pb_codegen_util.sp 
        "(** %s Generated Types and Encoding *)" 
        (Filename.basename proto_file_name); 
  F.empty_line sc; 
  print_ppx sc; 
  gen otypes  sc (List.map (fun m -> 
    let module C = (val m:Pb_codegen_sig.S) in 
    C.gen_sig, Some C.ocamldoc_title
  ) all_code_gen);
  
  F.line sc "module Make_decoder(Decoder:Pbrt_js.Decoder_sig) : sig";
  F.scope sc (fun sc -> 
    gen otypes sc [
      (
        Pb_codegen_decode_json.gen_sig , 
        Some (Pb_codegen_decode_json.ocamldoc_title)
      );]
  ); 
  F.line sc "end";

  F.line sc "module Make_encoder(Encoder:Pbrt_js.Encoder_sig) : sig";
  F.scope sc (fun sc -> 
    gen otypes sc [
      (
        Pb_codegen_encode_json.gen_sig , 
        Some (Pb_codegen_encode_json.ocamldoc_title)
      );]
  ); 
  F.line sc "end";
  output_string sig_oc (F.print sc);
  ()

(* -- main -- *)

let () = 

  let (
    proto_file_name, 
    include_dirs, 
    sig_oc, 
    struct_oc, 
    enable_debugging, 
    generated_files,
    cmd_line_files_options
  ) = parse_args () in 

  if enable_debugging
  then L.setup_from_out_channel stdout;

  let close_file_channels () = 
    close_out struct_oc; 
    close_out sig_oc
  in 

  try
    let cmd_line_files_options = 
      File_options.to_file_options cmd_line_files_options 
    in 

    let otypes, proto_file_options = 
      compile cmd_line_files_options include_dirs proto_file_name 
    in 

    generate_code sig_oc struct_oc otypes proto_file_options proto_file_name;

    close_file_channels ();

    List.iter (fun file_name ->
      Printf.printf "Generated %s\n" file_name; 
    ) generated_files

  with exn -> 
    close_file_channels ();
    List.iter (fun file_name ->
      Sys.remove file_name
    ) generated_files; 
    Printf.eprintf "%s\n" (Printexc.to_string exn);
    exit 1
