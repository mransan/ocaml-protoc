(** Generate all the code for a given OCaml module. *)

module Ot = Pb_codegen_ocaml_type
module Plugin = Pb_codegen_plugin
module F = Pb_codegen_formatting

type ocaml_mod = {
  ml: F.scope;
  mli: F.scope;
}

val codegen :
  Ot.proto ->
  generate_make:bool ->
  proto_file_options:Pb_raw_option.set ->
  proto_file_name:string ->
  services:bool ->
  Plugin.t list ->
  ocaml_mod
(** [codegen types] returns a full code listing for the [.ml]
    and the [.mli] files. *)
