(** Common module signature for all codegen modules *)

module type S = sig
  val gen_sig :
    ?and_:unit ->
    Pb_codegen_ocaml_type.type_ ->
    Pb_codegen_formatting.scope ->
    bool
  (** Generate a signature file (.mli) *)

  val gen_struct :
    ?and_:unit ->
    Pb_codegen_ocaml_type.type_ ->
    Pb_codegen_formatting.scope ->
    bool
  (** Generate the implementation (.ml) *)

  val ocamldoc_title : string
  (** OCamldoc title *)

  val file_suffix : string
  (** The suffix part of the generated file which will contain the
      struct and sig, if any (e.g. "pp" for the printer code) *)
end
