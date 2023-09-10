val gen_service_client_sig :
  Pb_codegen_ocaml_type.service -> Pb_codegen_formatting.scope -> unit

val gen_service_client_struct :
  Pb_codegen_ocaml_type.service -> Pb_codegen_formatting.scope -> unit

val gen_service_server_sig :
  Pb_codegen_ocaml_type.service -> Pb_codegen_formatting.scope -> unit

val gen_service_server_struct :
  Pb_codegen_ocaml_type.service -> Pb_codegen_formatting.scope -> unit
