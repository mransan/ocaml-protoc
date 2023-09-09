(** Plugin architecture.

    OCaml-protoc generates code based on a number of plugins,
    each of which can contribute code to the output files (.ml and .mli).
*)

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

type t = (module S)
(** A plugin is a code-generator respecting the signature {!S}. *)
