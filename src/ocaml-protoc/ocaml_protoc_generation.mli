(** Main entrypoint for code generation *)

module Ot = Pb_codegen_ocaml_type
module Cmdline = Ocaml_protoc_cmdline.Cmdline

val generate_code : Ot.type_ list list -> Pb_option.set -> Cmdline.t -> unit
