(** Plugin that generates values for QuickCheck tests. *)

include Pb_codegen_plugin.S

val plugin : Pb_codegen_plugin.t

(** {2 Aggregating all tests}

    In addition to generate a test for each message type, we also generate an
    aggregate values with all the available tests, so they're easy to run. *)

val gen_all_tests_sig :
  Pb_codegen_ocaml_type.type_ list -> Pb_codegen_formatting.scope -> unit

val gen_all_tests_struct :
  Pb_codegen_ocaml_type.type_ list -> Pb_codegen_formatting.scope -> unit
