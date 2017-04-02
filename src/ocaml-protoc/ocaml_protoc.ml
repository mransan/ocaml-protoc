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
module Codegen_util = Pb_codegen_util 

let sp = Codegen_util.sp 

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
  Codegen_util.caml_file_name_of_proto_file_name 

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
  let yojson = ref false in 
  let bs = ref false in 
  let binary = ref false in 
  let pp = ref false in 
  let include_dirs = ref [] in 
  let include_dirs_spec = (fun dir -> 
    include_dirs := dir :: (!include_dirs)
  ) in 
  let ml_out = ref "" in 
  let cmd_line_files_options = File_options.make () in 
   
  let cmd_line_args = [
    ("-debug", Arg.Set debug, "enable debugging");  
    ("-yojson", Arg.Set yojson, "generate yojson encoding");  
    ("-bs", Arg.Set bs, "generate BuckleScript encoding");  
    ("-binary", Arg.Set binary , "generate binary encoding");  
    ("-pp", Arg.Set pp, "generate pretty print functions");  
    ("-I", Arg.String include_dirs_spec, "include directories");  
    ("-ml_out", Arg.Set_string  ml_out, "output directory");  
  ] @ File_options.cmd_line_args cmd_line_files_options in 

  let anon_fun  = (fun proto_file -> 
    proto_file_name := proto_file
  )  in 

  let usage = "ocaml-protoc -ml_out <output_directory> <file_name>.proto" in

  Arg.parse cmd_line_args anon_fun usage;
  
  (* Maintain backward compatible behavior (ie if none of the new 
     switch are used then default to generating binary and pp *) 
  begin 
    if not !yojson && not !binary && not !pp && not !bs 
    then begin 
      binary := true;
      pp := true; 
    end;
  end;

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


  (
    !ml_out,
    !proto_file_name, 
    !include_dirs, 
    !debug,
    !binary,
    !yojson, 
    !bs, 
    !pp,
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

type codegen_f = ?and_:unit -> Ot.type_ -> F.scope -> bool 

let generate_for_all_types ocaml_types sc (f:codegen_f) ocamldoc_title = 
  begin
    match ocamldoc_title with
    | None -> () 
    | Some ocamldoc_title -> ( 
      F.empty_line sc;
      F.line sc @@ Codegen_util.sp 
          "(** {2 %s} *)" ocamldoc_title;  
      F.empty_line sc;
    )
  end;

  List.iter (fun types -> 
    let _:bool = List.fold_left (fun first type_ -> 
      let has_encoded = if first 
        then f type_ sc 
        else f ~and_:() type_ sc
      in 
      F.empty_line sc;
      first && (not has_encoded) 
    ) true types in 
    ()
  ) ocaml_types 

let open_files ~ml_out ~proto_file_name ~file_suffix = 
  let out_file_name = 
    let out_basename = 
      ocaml_file_name_of_proto_file_name ~proto_file_name ~file_suffix
    in 
    Filename.concat ml_out out_basename
  in 

  (open_out @@ out_file_name ^ ".mli" , open_out @@ out_file_name ^ ".ml")

let generate_type_and_default 
       ocaml_types proto_file_options ml_out proto_file_name = 
  let file_suffix = "types" in 
  let sig_oc, struct_oc = open_files ~ml_out ~proto_file_name ~file_suffix in 

  let print_ppx sc = 
    match Pb_option.get proto_file_options "ocaml_file_ppx" with
    | None -> () 
    | Some Pb_option.Constant_string s -> 
      F.line sc @@ Printf.sprintf "[@@@%s]" s
    | _ -> E.invalid_ppx_extension_option proto_file_name  
  in 

  (* .ml file *) 

  let sc = F.empty_scope () in 
  F.line sc "[@@@ocaml.warning \"-27-30-39\"]";
  F.empty_line sc; 
  print_ppx sc; 
  F.empty_line sc;
  generate_for_all_types ocaml_types sc 
    Pb_codegen_types.gen_struct None;
  generate_for_all_types ocaml_types sc 
    Pb_codegen_default.gen_struct None;

  output_string struct_oc (F.print sc);

  (* .mli file *)
  
  let sc = F.empty_scope () in 
  F.line sc @@ sp 
        "(** %s Types *)" 
        (Filename.basename proto_file_name); 
  F.empty_line sc; 
  print_ppx sc; 
  F.empty_line sc;
  generate_for_all_types ocaml_types sc 
    Pb_codegen_types.gen_sig 
    (Some Pb_codegen_types.ocamldoc_title);
  generate_for_all_types ocaml_types sc 
    Pb_codegen_default.gen_sig 
    (Some Pb_codegen_default.ocamldoc_title);

  output_string sig_oc (F.print sc)

let generate_mutable_records ocaml_types sc = 
  let ocaml_types = List.flatten ocaml_types in 
  List.iter (fun {Ot.spec; module_; _ } -> 
    match spec with
    | Ot.Record r -> 
      Pb_codegen_types.gen_type_record ~mutable_:() module_ r sc;
      F.empty_line sc;
      Pb_codegen_default.gen_default_record ~mutable_:() module_ r sc; 
    | _ -> () 
  ) ocaml_types

let generate_yojson ocaml_types ml_out proto_file_name = 
  let file_suffix = "yojson" in 
  let sig_oc, struct_oc = open_files ~ml_out ~proto_file_name ~file_suffix in 

  (* .ml file *) 
  let sc = F.empty_scope () in 
  F.line sc "[@@@ocaml.warning \"-27-30-39\"]";
  F.empty_line sc;
  generate_mutable_records ocaml_types sc;
  F.empty_line sc;

  generate_for_all_types ocaml_types sc 
    Pb_codegen_decode_yojson.gen_struct None; 
  generate_for_all_types ocaml_types sc 
    Pb_codegen_encode_yojson.gen_struct None; 
  output_string struct_oc (F.print sc);
  
  (* .mli file *)
  let sc = F.empty_scope () in 
  F.line sc @@ sp 
        "(** %s YoJSON Encoding *)" 
        (Filename.basename proto_file_name); 
  F.empty_line sc; 
  generate_for_all_types ocaml_types sc 
    Pb_codegen_encode_yojson.gen_sig 
    (Some Pb_codegen_encode_yojson.ocamldoc_title);
  generate_for_all_types ocaml_types sc 
    Pb_codegen_decode_yojson.gen_sig 
    (Some Pb_codegen_decode_yojson.ocamldoc_title);

  output_string sig_oc (F.print sc)

let generate_bs ocaml_types ml_out proto_file_name = 
  let file_suffix = "bs" in 
  let sig_oc, struct_oc = open_files ~ml_out ~proto_file_name ~file_suffix in 

  (* .ml file *) 
  let sc = F.empty_scope () in 
  F.line sc "[@@@ocaml.warning \"-27-30-39\"]";
  F.empty_line sc;
  generate_mutable_records ocaml_types sc;
  F.empty_line sc;

  generate_for_all_types ocaml_types sc 
    Pb_codegen_decode_bs.gen_struct None; 
  generate_for_all_types ocaml_types sc 
    Pb_codegen_encode_bs.gen_struct None; 
  output_string struct_oc (F.print sc);
  
  (* .mli file *)
  let sc = F.empty_scope () in 
  F.line sc @@ sp 
        "(** %s BuckleScript Encoding *)" 
        (Filename.basename proto_file_name); 
  F.empty_line sc; 
  generate_for_all_types ocaml_types sc 
    Pb_codegen_encode_bs.gen_sig 
    (Some Pb_codegen_encode_bs.ocamldoc_title);
  generate_for_all_types ocaml_types sc 
    Pb_codegen_decode_bs.gen_sig 
    (Some Pb_codegen_decode_bs.ocamldoc_title);

  output_string sig_oc (F.print sc)

let generate_binary ocaml_types ml_out proto_file_name = 
  let file_suffix = "pb" in 
  let sig_oc, struct_oc = open_files ~ml_out ~proto_file_name ~file_suffix in 

  (* .ml file *) 
  let sc = F.empty_scope () in 
  F.line sc "[@@@ocaml.warning \"-27-30-39\"]";
  F.empty_line sc;
  generate_mutable_records ocaml_types sc;
  F.empty_line sc;

  generate_for_all_types ocaml_types sc 
    Pb_codegen_decode_binary.gen_struct None; 
  generate_for_all_types ocaml_types sc 
    Pb_codegen_encode_binary.gen_struct None; 
  output_string struct_oc (F.print sc);
  
  (* .mli file *)
  let sc = F.empty_scope () in 
  F.line sc @@ sp 
        "(** %s Binary Encoding *)" 
        (Filename.basename proto_file_name); 
  F.empty_line sc; 
  generate_for_all_types ocaml_types sc 
    Pb_codegen_encode_binary.gen_sig 
    (Some Pb_codegen_encode_binary.ocamldoc_title);
  generate_for_all_types ocaml_types sc 
    Pb_codegen_decode_binary.gen_sig 
    (Some Pb_codegen_decode_binary.ocamldoc_title);

  output_string sig_oc (F.print sc)

let generate_pp ocaml_types ml_out proto_file_name = 
  let file_suffix = "pp" in 
  let sig_oc, struct_oc = open_files ~ml_out ~proto_file_name ~file_suffix in 

  (* .ml file *) 
  let sc = F.empty_scope () in 
  F.line sc "[@@@ocaml.warning \"-27-30-39\"]";
  F.empty_line sc;

  generate_for_all_types ocaml_types sc 
    Pb_codegen_pp.gen_struct None; 
  output_string struct_oc (F.print sc);
  
  (* .mli file *)
  let sc = F.empty_scope () in 
  F.line sc @@ sp 
        "(** %s Pretty Printing *)" 
        (Filename.basename proto_file_name); 
  F.empty_line sc; 
  generate_for_all_types ocaml_types sc 
    Pb_codegen_pp.gen_sig 
    (Some Pb_codegen_pp.ocamldoc_title);

  output_string sig_oc (F.print sc)

let generate_code 
      ocaml_types 
      proto_file_options 
      ml_out
      proto_file_name 
      enable_binary
      enable_yojson 
      enable_bs
      enable_pp = 

  generate_type_and_default 
    ocaml_types proto_file_options ml_out proto_file_name;

  begin if enable_yojson
  then generate_yojson ocaml_types ml_out proto_file_name
  end; 
  
  begin if enable_binary
  then generate_binary ocaml_types ml_out proto_file_name
  end; 
  
  begin if enable_pp
  then generate_pp ocaml_types ml_out proto_file_name
  end; 
  
  begin if enable_bs
  then generate_bs ocaml_types ml_out proto_file_name
  end; 
  ()


(* -- main -- *)

let () = 

  let (
    ml_out,
    proto_file_name, 
    include_dirs, 
    enable_debugging, 
    generate_binary,
    generate_yojson,
    generate_bsjson,
    generate_pp,
    cmd_line_files_options
  ) = parse_args () in 

  if enable_debugging
  then L.setup_from_out_channel stdout;

  try
    let cmd_line_files_options = 
      File_options.to_file_options cmd_line_files_options 
    in 

    let ocaml_types, proto_file_options = 
      compile cmd_line_files_options include_dirs proto_file_name 
    in 
  
    let proto_file_name = Filename.basename proto_file_name in 

    generate_code 
      ocaml_types 
      proto_file_options 
      ml_out
      proto_file_name 
      generate_binary
      generate_yojson
      generate_bsjson
      generate_pp;

(*    List.iter (fun file_name ->
      Printf.printf "Generated %s\n" file_name; 
    ) generated_files*)

  with exn -> 
(*    List.iter (fun file_name ->
      Sys.remove file_name
    ) generated_files; *)
    Printf.eprintf "%s\n" (Printexc.to_string exn);
    exit 1
