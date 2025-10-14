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

module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
module Codegen_util = Pb_codegen_util
module Cmdline = Ocaml_protoc_cmdline.Cmdline
module CG_all = Pb_codegen_all

let open_files cmdline (f : ml:out_channel -> mli:out_channel -> 'a) : 'a =
  let { Cmdline.ml_out; Cmdline.proto_file_name; _ } = cmdline in

  let out_file_name =
    let proto_file_name = Filename.basename proto_file_name in
    let out_basename =
      Codegen_util.caml_file_name_of_proto_file_name ~proto_file_name
    in
    Filename.concat ml_out out_basename
  in

  Pb_logger.log "Generating %s.mli\n%!" out_file_name;
  Pb_logger.log "Generating %s.ml\n%!" out_file_name;

  let oc_mli = open_out @@ out_file_name ^ ".mli" in
  let oc_ml = open_out @@ out_file_name ^ ".ml" in

  Pb_util.protect
    ~finally:(fun () ->
      close_out oc_mli;
      close_out oc_ml)
    (fun () -> f ~ml:oc_ml ~mli:oc_mli)

let generate_code ocaml_types ~proto_file_options cmdline : unit =
  let plugins : Pb_codegen_plugin.t list =
    List.flatten
      [
        (if !(cmdline.Cmdline.dump_type_repr) then
           [ Pb_codegen_ocaml_type_dump.plugin ]
         else
           []);
        (if !(cmdline.Cmdline.pp) then
           [ Pb_codegen_pp.plugin ]
         else
           []);
        (if !(cmdline.Cmdline.binary) then
           [ Pb_codegen_encode_binary.plugin; Pb_codegen_decode_binary.plugin ]
         else
           []);
        (if !(cmdline.Cmdline.yojson) then
           [ Pb_codegen_encode_yojson.plugin; Pb_codegen_decode_yojson.plugin ]
         else
           []);
        (if !(cmdline.Cmdline.bs) then
           [ Pb_codegen_encode_bs.plugin; Pb_codegen_decode_bs.plugin ]
         else
           []);
      ]
  in

  let services = !(cmdline.Cmdline.services) in

  let ocaml_mod : CG_all.ocaml_mod =
    CG_all.codegen ocaml_types ~generate_make:!(cmdline.make)
      ~proto_file_options ~proto_file_name:cmdline.proto_file_name ~services
      plugins
  in

  (* now write the files *)
  open_files cmdline (fun ~ml ~mli ->
      F.output ml ocaml_mod.ml;
      F.output mli ocaml_mod.mli);
  ()
