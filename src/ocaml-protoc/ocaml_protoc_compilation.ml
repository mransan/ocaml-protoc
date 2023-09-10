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

module E = Pb_exception
module Loc = Pb_location
module Pt = Pb_parsing_parse_tree
module Parsing_util = Pb_parsing_util
module Tt = Pb_typing_type_tree
module Typing_util = Pb_typing_util
module Ot = Pb_codegen_ocaml_type
module Cmdline = Ocaml_protoc_cmdline.Cmdline

let find_imported_file include_dirs file_name =
  if Sys.file_exists file_name then
    file_name
  else (
    let found_file =
      List.fold_left
        (fun found_file include_dir ->
          let try_file_name = Filename.concat include_dir file_name in
          match found_file, Sys.file_exists try_file_name with
          | None, true -> Some try_file_name
          | Some previous, true ->
            Printf.eprintf
              ("[Warning] Imported file %s found in 2 directories, "
             ^^ "picking: %s\n")
              file_name previous;
            found_file
          | _, false -> found_file)
        None include_dirs
    in

    match found_file with
    | None -> E.import_file_not_found file_name
    | Some file_name -> file_name
  )

let compile cmdline cmd_line_files_options : Ot.proto * _ =
  let { Cmdline.include_dirs; proto_file_name; unsigned_tag; _ } = cmdline in

  (* parsing *)
  let protos =
    Pb_parsing.parse_file
      (fun file_name ->
        let file_name = find_imported_file include_dirs file_name in
        file_name, Pb_util.read_file file_name)
      proto_file_name
  in

  (* file options can be overriden/added with command line arguments *)
  let protos =
    List.map
      (fun proto ->
        {
          proto with
          Pt.file_options =
            Pb_option.merge proto.Pt.file_options cmd_line_files_options;
        })
      protos
  in

  let proto_file_options =
    let main_proto = List.hd protos in
    main_proto.Pt.file_options
  in

  (* typing *)
  let typed_proto = Pb_typing.perform_typing protos in
  let all_typed_protos = List.flatten typed_proto.proto_types in

  (* Only get the types which are part of the given proto file
     (compilation unit) *)
  let typed_proto =
    {
      typed_proto with
      Tt.proto_types =
        List.filter
          (function
            | { Tt.file_name; _ } :: _ when file_name = proto_file_name -> true
            | _ -> false)
          typed_proto.proto_types;
    }
  in

  (* -- OCaml Backend -- *)
  let module BO = Pb_codegen_backend in
  let ocaml_proto =
    BO.compile ~unsigned_tag:!unsigned_tag ~all_types:all_typed_protos
      typed_proto
  in
  ocaml_proto, proto_file_options
