(** Common utility functions for OCaml code generation *)

val sp : ('a, unit, string) format -> 'a
(** [sp x] same as sprintf but prefixed with new line *)

val let_decl_of_and : 'a option -> string
(** [let_decl_of_and and_] returns the function declaration ["let"] (when ?and_
   is [None]), ["and"] otherwise.
 *)

val string_of_record_field_type :
  ?module_prefix:string ->
  Pb_codegen_ocaml_type.record_field_type ->
  string

val string_of_basic_type : Pb_codegen_ocaml_type.basic_type -> string

val string_of_field_type :
  ?module_prefix:string ->
  Pb_codegen_ocaml_type.field_type ->
  string

val function_name_of_user_defined :
  function_prefix:string ->
  module_suffix:string ->
  Pb_codegen_ocaml_type.user_defined_type ->
  string
(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)<prefix>_(type_name)`.

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each
    user defined field type.
 *)

val caml_file_name_of_proto_file_name :
  proto_file_name:string ->
  file_suffix:string ->
  string
(** [caml_file_name_of_proto_file_name filename] returns the OCaml file name from
    the protobuf file name
 *)

val mutable_record_name : string -> string
(** [mutable_record_name record_name] returns the type name of the `mutable`
    type name. We use mutable types when decoding for better performance,
    this function encapsulate the nameing convention for this additional
    type.
 *)

val string_of_payload_kind :
  ?capitalize:unit ->
  Pb_codegen_ocaml_type.payload_kind ->
  bool ->
  string
(** [string_of_payload_kind ~capitalize:() payload_kind packed] will return the
    string corresponding to the payload kind.
 *)

val camel_case_of_label : string -> string
(** this function transforms a `lower_case_like_this` into an
    camlCaseLikeThis *)

val camel_case_of_constructor : string -> string
(** this function transform an OCaml constructuror `Like_this` into
    a 'likeThis' case *)

val collect_modules_of_types :
  Pb_codegen_ocaml_type.type_ list ->
  string list
(** [collect_modules_of_types ocaml_types] return the list of all the modules
    that the [ocaml_types] depends on *)
