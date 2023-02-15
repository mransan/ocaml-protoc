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
module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
module Codegen_util = Pb_codegen_util

let sp = Codegen_util.sp

module Cmdline = Ocaml_protoc_cmdline.Cmdline
module File_options = Ocaml_protoc_cmdline.File_options
module Compilation = Ocaml_protoc_compilation
module Generation = Ocaml_protoc_generation

(* -- main -- *)

let () =
  let cmdline = Cmdline.parse () in

  try
    let cmd_line_file_options =
      File_options.to_file_options cmdline.Cmdline.cmd_line_file_options
    in

    let ocaml_types, proto_file_options =
      Compilation.compile cmdline cmd_line_file_options
    in

    Generation.generate_code ocaml_types proto_file_options cmdline
  with exn ->
    Printf.eprintf "%s\n" (Printexc.to_string exn);
    exit 1
