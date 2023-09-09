(** Code generator for the [default] functions (i.e builders) *)

include Pb_codegen_plugin.S

val gen_record_mutable :
  gen_file_suffix:string ->
  module_prefix:string ->
  Pb_codegen_ocaml_type.record ->
  Pb_codegen_formatting.scope ->
  unit

val plugin : with_mutable_records:bool -> unit -> Pb_codegen_plugin.t
