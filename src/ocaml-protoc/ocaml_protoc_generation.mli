(** Main entrypoint for code generation *)

module Ot = Pb_codegen_ocaml_type
module Cmdline = Ocaml_protoc_cmdline.Cmdline

val generate_code :
  Ot.proto -> proto_file_options:Pb_option.set -> Cmdline.t -> unit
