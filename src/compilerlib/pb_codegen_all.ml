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
module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting
module Plugin = Pb_codegen_plugin

type codegen_f = Plugin.codegen_f
type codegen_service_f = Ot.service -> F.scope -> unit

type ocaml_mod = {
  ml: F.scope;
  mli: F.scope;
}

let new_ocaml_mod ~proto_file_options ~proto_file_name () : ocaml_mod =
  let self = { ml = F.empty_scope (); mli = F.empty_scope () } in

  let print_ppx sc =
    match Pb_raw_option.get_ext proto_file_options "ocaml_file_ppx" with
    | None -> ()
    | Some Pb_option.(Scalar_value (Constant_string s)) ->
      F.linep sc "[@@@%s]" s
    | _ -> E.invalid_ppx_extension_option proto_file_name
  in

  (* write preludes *)
  F.line self.ml "[@@@ocaml.warning \"-27-30-39-44\"]";
  F.empty_line self.ml;
  print_ppx self.ml;
  F.empty_line self.mli;

  F.linep self.mli "(** Code for %s *)" (Filename.basename proto_file_name);
  F.empty_line self.mli;
  F.linep self.mli "(* generated from %S, do not edit *)" proto_file_name;
  F.empty_line self.mli;
  print_ppx self.mli;
  F.empty_line self.mli;

  self

let generate_for_all_types (ocaml_types : Ot.type_ list list) (sc : F.scope)
    (f : codegen_f) ocamldoc_title : unit =
  (match ocamldoc_title with
  | None -> ()
  | Some ocamldoc_title ->
    F.empty_line sc;
    F.linep sc "(** {2 %s} *)" ocamldoc_title;
    F.empty_line sc);

  List.iter
    (fun types ->
      let (_ : bool) =
        List.fold_left
          (fun first type_ ->
            let has_encoded =
              if first then
                f type_ sc
              else
                f ~and_:() type_ sc
            in
            F.empty_line sc;
            first && not has_encoded)
          true types
      in
      ())
    ocaml_types

let generate_for_all_services (services : Ot.service list) (sc : F.scope)
    (f : codegen_service_f) ocamldoc_title : unit =
  (match ocamldoc_title with
  | None -> ()
  | Some ocamldoc_title ->
    F.empty_line sc;
    F.linep sc "(** {2 %s} *)" ocamldoc_title;
    F.empty_line sc);

  List.iter (fun (service : Ot.service) -> f service sc) services

let generate_type_and_default (self : ocaml_mod) ocaml_types : unit =
  generate_for_all_types ocaml_types self.ml Pb_codegen_types.gen_struct None;
  generate_for_all_types ocaml_types self.ml Pb_codegen_default.gen_struct None;
  generate_for_all_types ocaml_types self.mli Pb_codegen_types.gen_sig
    (Some Pb_codegen_types.ocamldoc_title);
  generate_for_all_types ocaml_types self.mli Pb_codegen_default.gen_sig
    (Some Pb_codegen_default.ocamldoc_title);
  ()

let generate_make (self : ocaml_mod) ocaml_types : unit =
  generate_for_all_types ocaml_types self.ml Pb_codegen_make.gen_struct
    (Some Pb_codegen_make.ocamldoc_title);
  generate_for_all_types ocaml_types self.mli Pb_codegen_make.gen_sig
    (Some Pb_codegen_make.ocamldoc_title)

let generate_mutable_records (self : ocaml_mod) ocaml_types : unit =
  let ocaml_types = List.flatten ocaml_types in
  List.iter
    (fun { Ot.spec; _ } ->
      match spec with
      | Ot.Record r ->
        Pb_codegen_types.gen_record_mutable r self.ml;
        F.empty_line self.ml;
        Pb_codegen_default.gen_record_mutable r self.ml;
        F.empty_line self.ml
      | _ -> ())
    ocaml_types

let generate_service_struct service sc : unit =
  Pb_codegen_services.gen_service_struct service sc

let generate_service_sig service sc : unit =
  Pb_codegen_services.gen_service_sig service sc

let generate_services (self : ocaml_mod) services : unit =
  generate_for_all_services services self.ml generate_service_struct None;
  generate_for_all_services services self.mli generate_service_sig
    (Some "Services")

let generate_plugin (self : ocaml_mod) ocaml_types (p : Plugin.t) : unit =
  let (module P) = p in
  F.line self.ml "[@@@ocaml.warning \"-27-30-39\"]";
  generate_for_all_types ocaml_types self.ml P.gen_struct
    (Some P.ocamldoc_title);
  generate_for_all_types ocaml_types self.mli P.gen_sig (Some P.ocamldoc_title);
  ()

let codegen (proto : Ot.proto) ~generate_make:gen_make ~proto_file_options
    ~proto_file_name ~services (plugins : Plugin.t list) : ocaml_mod =
  let self = new_ocaml_mod ~proto_file_options ~proto_file_name () in
  generate_type_and_default self proto.proto_types;
  if List.exists Pb_codegen_plugin.requires_mutable_records plugins then
    generate_mutable_records self proto.proto_types;
  if gen_make then generate_make self proto.proto_types;
  List.iter (generate_plugin self proto.proto_types) plugins;

  (* services come last, they need binary and json *)
  if services then generate_services self proto.proto_services;
  self
