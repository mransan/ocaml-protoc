
(** Code for descriptor.proto *)

(* generated from "src/tests/integration-tests/google/protobuf/descriptor.proto", do not edit *)



(** {2 Types} *)

type field_descriptor_proto_label =
  | Label_optional 
  | Label_required 
  | Label_repeated 

type field_descriptor_proto_type =
  | Type_double 
  | Type_float 
  | Type_int64 
  | Type_uint64 
  | Type_int32 
  | Type_fixed64 
  | Type_fixed32 
  | Type_bool 
  | Type_string 
  | Type_group 
  | Type_message 
  | Type_bytes 
  | Type_uint32 
  | Type_enum 
  | Type_sfixed32 
  | Type_sfixed64 
  | Type_sint32 
  | Type_sint64 

type field_options_ctype =
  | String 
  | Cord 
  | String_piece 

type field_options_jstype =
  | Js_normal 
  | Js_string 
  | Js_number 

type uninterpreted_option_name_part = private {
  mutable name_part : string;
  mutable is_extension : bool;
}

type uninterpreted_option = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 6 fields *)
  mutable name : uninterpreted_option_name_part list;
  mutable identifier_value : string;
  mutable positive_int_value : int64;
  mutable negative_int_value : int64;
  mutable double_value : float;
  mutable string_value : bytes;
  mutable aggregate_value : string;
}

type field_options = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 7 fields *)
  mutable ctype : field_options_ctype;
  mutable packed : bool;
  mutable jstype : field_options_jstype;
  mutable lazy_ : bool;
  mutable unverified_lazy : bool;
  mutable deprecated : bool;
  mutable weak : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type field_descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 10 fields *)
  mutable name : string;
  mutable number : int32;
  mutable label : field_descriptor_proto_label;
  mutable type_ : field_descriptor_proto_type;
  mutable type_name : string;
  mutable extendee : string;
  mutable default_value : string;
  mutable oneof_index : int32;
  mutable json_name : string;
  mutable options : field_options option;
  mutable proto3_optional : bool;
}

type enum_value_options = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable deprecated : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type enum_value_descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable name : string;
  mutable number : int32;
  mutable options : enum_value_options option;
}

type enum_options = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable allow_alias : bool;
  mutable deprecated : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type enum_descriptor_proto_enum_reserved_range = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable start : int32;
  mutable end_ : int32;
}

type enum_descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable value : enum_value_descriptor_proto list;
  mutable options : enum_options option;
  mutable reserved_range : enum_descriptor_proto_enum_reserved_range list;
  mutable reserved_name : string list;
}

type extension_range_options = private {
  mutable uninterpreted_option : uninterpreted_option list;
}

type descriptor_proto_extension_range = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable start : int32;
  mutable end_ : int32;
  mutable options : extension_range_options option;
}

type oneof_options = private {
  mutable uninterpreted_option : uninterpreted_option list;
}

type oneof_descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable options : oneof_options option;
}

type message_options = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 4 fields *)
  mutable message_set_wire_format : bool;
  mutable no_standard_descriptor_accessor : bool;
  mutable deprecated : bool;
  mutable map_entry : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type descriptor_proto_reserved_range = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable start : int32;
  mutable end_ : int32;
}

type descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable field : field_descriptor_proto list;
  mutable extension : field_descriptor_proto list;
  mutable nested_type : descriptor_proto list;
  mutable enum_type : enum_descriptor_proto list;
  mutable extension_range : descriptor_proto_extension_range list;
  mutable oneof_decl : oneof_descriptor_proto list;
  mutable options : message_options option;
  mutable reserved_range : descriptor_proto_reserved_range list;
  mutable reserved_name : string list;
}

type method_options_idempotency_level =
  | Idempotency_unknown 
  | No_side_effects 
  | Idempotent 

type method_options = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable deprecated : bool;
  mutable idempotency_level : method_options_idempotency_level;
  mutable uninterpreted_option : uninterpreted_option list;
}

type method_descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable name : string;
  mutable input_type : string;
  mutable output_type : string;
  mutable options : method_options option;
  mutable client_streaming : bool;
  mutable server_streaming : bool;
}

type service_options = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable deprecated : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type service_descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable method_ : method_descriptor_proto list;
  mutable options : service_options option;
}

type file_options_optimize_mode =
  | Speed 
  | Code_size 
  | Lite_runtime 

type file_options = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 20 fields *)
  mutable java_package : string;
  mutable java_outer_classname : string;
  mutable java_multiple_files : bool;
  mutable java_generate_equals_and_hash : bool;
  mutable java_string_check_utf8 : bool;
  mutable optimize_for : file_options_optimize_mode;
  mutable go_package : string;
  mutable cc_generic_services : bool;
  mutable java_generic_services : bool;
  mutable py_generic_services : bool;
  mutable php_generic_services : bool;
  mutable deprecated : bool;
  mutable cc_enable_arenas : bool;
  mutable objc_class_prefix : string;
  mutable csharp_namespace : string;
  mutable swift_prefix : string;
  mutable php_class_prefix : string;
  mutable php_namespace : string;
  mutable php_metadata_namespace : string;
  mutable ruby_package : string;
  mutable uninterpreted_option : uninterpreted_option list;
}

type source_code_info_location = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable path : int32 list;
  mutable span : int32 list;
  mutable leading_comments : string;
  mutable trailing_comments : string;
  mutable leading_detached_comments : string list;
}

type source_code_info = private {
  mutable location : source_code_info_location list;
}

type file_descriptor_proto = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable name : string;
  mutable package : string;
  mutable dependency : string list;
  mutable public_dependency : int32 list;
  mutable weak_dependency : int32 list;
  mutable message_type : descriptor_proto list;
  mutable enum_type : enum_descriptor_proto list;
  mutable service : service_descriptor_proto list;
  mutable extension : field_descriptor_proto list;
  mutable options : file_options option;
  mutable source_code_info : source_code_info option;
  mutable syntax : string;
}

type file_descriptor_set = private {
  mutable file : file_descriptor_proto list;
}

type generated_code_info_annotation = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable path : int32 list;
  mutable source_file : string;
  mutable begin_ : int32;
  mutable end_ : int32;
}

type generated_code_info = private {
  mutable annotation : generated_code_info_annotation list;
}


(** {2 Basic values} *)

val default_field_descriptor_proto_label : unit -> field_descriptor_proto_label
(** [default_field_descriptor_proto_label ()] is a new empty value for type [field_descriptor_proto_label] *)

val default_field_descriptor_proto_type : unit -> field_descriptor_proto_type
(** [default_field_descriptor_proto_type ()] is a new empty value for type [field_descriptor_proto_type] *)

val default_field_options_ctype : unit -> field_options_ctype
(** [default_field_options_ctype ()] is a new empty value for type [field_options_ctype] *)

val default_field_options_jstype : unit -> field_options_jstype
(** [default_field_options_jstype ()] is a new empty value for type [field_options_jstype] *)

val default_uninterpreted_option_name_part : unit -> uninterpreted_option_name_part 
(** [default_uninterpreted_option_name_part ()] is a new empty value for type [uninterpreted_option_name_part] *)

val default_uninterpreted_option : unit -> uninterpreted_option 
(** [default_uninterpreted_option ()] is a new empty value for type [uninterpreted_option] *)

val default_field_options : unit -> field_options 
(** [default_field_options ()] is a new empty value for type [field_options] *)

val default_field_descriptor_proto : unit -> field_descriptor_proto 
(** [default_field_descriptor_proto ()] is a new empty value for type [field_descriptor_proto] *)

val default_enum_value_options : unit -> enum_value_options 
(** [default_enum_value_options ()] is a new empty value for type [enum_value_options] *)

val default_enum_value_descriptor_proto : unit -> enum_value_descriptor_proto 
(** [default_enum_value_descriptor_proto ()] is a new empty value for type [enum_value_descriptor_proto] *)

val default_enum_options : unit -> enum_options 
(** [default_enum_options ()] is a new empty value for type [enum_options] *)

val default_enum_descriptor_proto_enum_reserved_range : unit -> enum_descriptor_proto_enum_reserved_range 
(** [default_enum_descriptor_proto_enum_reserved_range ()] is a new empty value for type [enum_descriptor_proto_enum_reserved_range] *)

val default_enum_descriptor_proto : unit -> enum_descriptor_proto 
(** [default_enum_descriptor_proto ()] is a new empty value for type [enum_descriptor_proto] *)

val default_extension_range_options : unit -> extension_range_options 
(** [default_extension_range_options ()] is a new empty value for type [extension_range_options] *)

val default_descriptor_proto_extension_range : unit -> descriptor_proto_extension_range 
(** [default_descriptor_proto_extension_range ()] is a new empty value for type [descriptor_proto_extension_range] *)

val default_oneof_options : unit -> oneof_options 
(** [default_oneof_options ()] is a new empty value for type [oneof_options] *)

val default_oneof_descriptor_proto : unit -> oneof_descriptor_proto 
(** [default_oneof_descriptor_proto ()] is a new empty value for type [oneof_descriptor_proto] *)

val default_message_options : unit -> message_options 
(** [default_message_options ()] is a new empty value for type [message_options] *)

val default_descriptor_proto_reserved_range : unit -> descriptor_proto_reserved_range 
(** [default_descriptor_proto_reserved_range ()] is a new empty value for type [descriptor_proto_reserved_range] *)

val default_descriptor_proto : unit -> descriptor_proto 
(** [default_descriptor_proto ()] is a new empty value for type [descriptor_proto] *)

val default_method_options_idempotency_level : unit -> method_options_idempotency_level
(** [default_method_options_idempotency_level ()] is a new empty value for type [method_options_idempotency_level] *)

val default_method_options : unit -> method_options 
(** [default_method_options ()] is a new empty value for type [method_options] *)

val default_method_descriptor_proto : unit -> method_descriptor_proto 
(** [default_method_descriptor_proto ()] is a new empty value for type [method_descriptor_proto] *)

val default_service_options : unit -> service_options 
(** [default_service_options ()] is a new empty value for type [service_options] *)

val default_service_descriptor_proto : unit -> service_descriptor_proto 
(** [default_service_descriptor_proto ()] is a new empty value for type [service_descriptor_proto] *)

val default_file_options_optimize_mode : unit -> file_options_optimize_mode
(** [default_file_options_optimize_mode ()] is a new empty value for type [file_options_optimize_mode] *)

val default_file_options : unit -> file_options 
(** [default_file_options ()] is a new empty value for type [file_options] *)

val default_source_code_info_location : unit -> source_code_info_location 
(** [default_source_code_info_location ()] is a new empty value for type [source_code_info_location] *)

val default_source_code_info : unit -> source_code_info 
(** [default_source_code_info ()] is a new empty value for type [source_code_info] *)

val default_file_descriptor_proto : unit -> file_descriptor_proto 
(** [default_file_descriptor_proto ()] is a new empty value for type [file_descriptor_proto] *)

val default_file_descriptor_set : unit -> file_descriptor_set 
(** [default_file_descriptor_set ()] is a new empty value for type [file_descriptor_set] *)

val default_generated_code_info_annotation : unit -> generated_code_info_annotation 
(** [default_generated_code_info_annotation ()] is a new empty value for type [generated_code_info_annotation] *)

val default_generated_code_info : unit -> generated_code_info 
(** [default_generated_code_info ()] is a new empty value for type [generated_code_info] *)


(** {2 Make functions} *)

val make_uninterpreted_option_name_part : 
  name_part:string ->
  is_extension:bool ->
  unit ->
  uninterpreted_option_name_part
(** [make_uninterpreted_option_name_part … ()] is a builder for type [uninterpreted_option_name_part] *)

val copy_uninterpreted_option_name_part : uninterpreted_option_name_part -> uninterpreted_option_name_part

val uninterpreted_option_name_part_set_name_part : uninterpreted_option_name_part -> string -> unit
  (** set field name_part in uninterpreted_option_name_part *)

val uninterpreted_option_name_part_set_is_extension : uninterpreted_option_name_part -> bool -> unit
  (** set field is_extension in uninterpreted_option_name_part *)

val make_uninterpreted_option : 
  ?name:uninterpreted_option_name_part list ->
  ?identifier_value:string ->
  ?positive_int_value:int64 ->
  ?negative_int_value:int64 ->
  ?double_value:float ->
  ?string_value:bytes ->
  ?aggregate_value:string ->
  unit ->
  uninterpreted_option
(** [make_uninterpreted_option … ()] is a builder for type [uninterpreted_option] *)

val copy_uninterpreted_option : uninterpreted_option -> uninterpreted_option

val uninterpreted_option_set_name : uninterpreted_option -> uninterpreted_option_name_part list -> unit
  (** set field name in uninterpreted_option *)

val uninterpreted_option_has_identifier_value : uninterpreted_option -> bool
  (** presence of field "identifier_value" in [uninterpreted_option] *)

val uninterpreted_option_set_identifier_value : uninterpreted_option -> string -> unit
  (** set field identifier_value in uninterpreted_option *)

val uninterpreted_option_has_positive_int_value : uninterpreted_option -> bool
  (** presence of field "positive_int_value" in [uninterpreted_option] *)

val uninterpreted_option_set_positive_int_value : uninterpreted_option -> int64 -> unit
  (** set field positive_int_value in uninterpreted_option *)

val uninterpreted_option_has_negative_int_value : uninterpreted_option -> bool
  (** presence of field "negative_int_value" in [uninterpreted_option] *)

val uninterpreted_option_set_negative_int_value : uninterpreted_option -> int64 -> unit
  (** set field negative_int_value in uninterpreted_option *)

val uninterpreted_option_has_double_value : uninterpreted_option -> bool
  (** presence of field "double_value" in [uninterpreted_option] *)

val uninterpreted_option_set_double_value : uninterpreted_option -> float -> unit
  (** set field double_value in uninterpreted_option *)

val uninterpreted_option_has_string_value : uninterpreted_option -> bool
  (** presence of field "string_value" in [uninterpreted_option] *)

val uninterpreted_option_set_string_value : uninterpreted_option -> bytes -> unit
  (** set field string_value in uninterpreted_option *)

val uninterpreted_option_has_aggregate_value : uninterpreted_option -> bool
  (** presence of field "aggregate_value" in [uninterpreted_option] *)

val uninterpreted_option_set_aggregate_value : uninterpreted_option -> string -> unit
  (** set field aggregate_value in uninterpreted_option *)

val make_field_options : 
  ?ctype:field_options_ctype ->
  ?packed:bool ->
  ?jstype:field_options_jstype ->
  ?lazy_:bool ->
  ?unverified_lazy:bool ->
  ?deprecated:bool ->
  ?weak:bool ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  field_options
(** [make_field_options … ()] is a builder for type [field_options] *)

val copy_field_options : field_options -> field_options

val field_options_has_ctype : field_options -> bool
  (** presence of field "ctype" in [field_options] *)

val field_options_set_ctype : field_options -> field_options_ctype -> unit
  (** set field ctype in field_options *)

val field_options_has_packed : field_options -> bool
  (** presence of field "packed" in [field_options] *)

val field_options_set_packed : field_options -> bool -> unit
  (** set field packed in field_options *)

val field_options_has_jstype : field_options -> bool
  (** presence of field "jstype" in [field_options] *)

val field_options_set_jstype : field_options -> field_options_jstype -> unit
  (** set field jstype in field_options *)

val field_options_has_lazy_ : field_options -> bool
  (** presence of field "lazy_" in [field_options] *)

val field_options_set_lazy_ : field_options -> bool -> unit
  (** set field lazy_ in field_options *)

val field_options_has_unverified_lazy : field_options -> bool
  (** presence of field "unverified_lazy" in [field_options] *)

val field_options_set_unverified_lazy : field_options -> bool -> unit
  (** set field unverified_lazy in field_options *)

val field_options_has_deprecated : field_options -> bool
  (** presence of field "deprecated" in [field_options] *)

val field_options_set_deprecated : field_options -> bool -> unit
  (** set field deprecated in field_options *)

val field_options_has_weak : field_options -> bool
  (** presence of field "weak" in [field_options] *)

val field_options_set_weak : field_options -> bool -> unit
  (** set field weak in field_options *)

val field_options_set_uninterpreted_option : field_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in field_options *)

val make_field_descriptor_proto : 
  ?name:string ->
  ?number:int32 ->
  ?label:field_descriptor_proto_label ->
  ?type_:field_descriptor_proto_type ->
  ?type_name:string ->
  ?extendee:string ->
  ?default_value:string ->
  ?oneof_index:int32 ->
  ?json_name:string ->
  ?options:field_options ->
  ?proto3_optional:bool ->
  unit ->
  field_descriptor_proto
(** [make_field_descriptor_proto … ()] is a builder for type [field_descriptor_proto] *)

val copy_field_descriptor_proto : field_descriptor_proto -> field_descriptor_proto

val field_descriptor_proto_has_name : field_descriptor_proto -> bool
  (** presence of field "name" in [field_descriptor_proto] *)

val field_descriptor_proto_set_name : field_descriptor_proto -> string -> unit
  (** set field name in field_descriptor_proto *)

val field_descriptor_proto_has_number : field_descriptor_proto -> bool
  (** presence of field "number" in [field_descriptor_proto] *)

val field_descriptor_proto_set_number : field_descriptor_proto -> int32 -> unit
  (** set field number in field_descriptor_proto *)

val field_descriptor_proto_has_label : field_descriptor_proto -> bool
  (** presence of field "label" in [field_descriptor_proto] *)

val field_descriptor_proto_set_label : field_descriptor_proto -> field_descriptor_proto_label -> unit
  (** set field label in field_descriptor_proto *)

val field_descriptor_proto_has_type_ : field_descriptor_proto -> bool
  (** presence of field "type_" in [field_descriptor_proto] *)

val field_descriptor_proto_set_type_ : field_descriptor_proto -> field_descriptor_proto_type -> unit
  (** set field type_ in field_descriptor_proto *)

val field_descriptor_proto_has_type_name : field_descriptor_proto -> bool
  (** presence of field "type_name" in [field_descriptor_proto] *)

val field_descriptor_proto_set_type_name : field_descriptor_proto -> string -> unit
  (** set field type_name in field_descriptor_proto *)

val field_descriptor_proto_has_extendee : field_descriptor_proto -> bool
  (** presence of field "extendee" in [field_descriptor_proto] *)

val field_descriptor_proto_set_extendee : field_descriptor_proto -> string -> unit
  (** set field extendee in field_descriptor_proto *)

val field_descriptor_proto_has_default_value : field_descriptor_proto -> bool
  (** presence of field "default_value" in [field_descriptor_proto] *)

val field_descriptor_proto_set_default_value : field_descriptor_proto -> string -> unit
  (** set field default_value in field_descriptor_proto *)

val field_descriptor_proto_has_oneof_index : field_descriptor_proto -> bool
  (** presence of field "oneof_index" in [field_descriptor_proto] *)

val field_descriptor_proto_set_oneof_index : field_descriptor_proto -> int32 -> unit
  (** set field oneof_index in field_descriptor_proto *)

val field_descriptor_proto_has_json_name : field_descriptor_proto -> bool
  (** presence of field "json_name" in [field_descriptor_proto] *)

val field_descriptor_proto_set_json_name : field_descriptor_proto -> string -> unit
  (** set field json_name in field_descriptor_proto *)

val field_descriptor_proto_has_options : field_descriptor_proto -> bool
  (** presence of field "options" in [field_descriptor_proto] *)

val field_descriptor_proto_set_options : field_descriptor_proto -> field_options -> unit
  (** set field options in field_descriptor_proto *)

val field_descriptor_proto_has_proto3_optional : field_descriptor_proto -> bool
  (** presence of field "proto3_optional" in [field_descriptor_proto] *)

val field_descriptor_proto_set_proto3_optional : field_descriptor_proto -> bool -> unit
  (** set field proto3_optional in field_descriptor_proto *)

val make_enum_value_options : 
  ?deprecated:bool ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  enum_value_options
(** [make_enum_value_options … ()] is a builder for type [enum_value_options] *)

val copy_enum_value_options : enum_value_options -> enum_value_options

val enum_value_options_has_deprecated : enum_value_options -> bool
  (** presence of field "deprecated" in [enum_value_options] *)

val enum_value_options_set_deprecated : enum_value_options -> bool -> unit
  (** set field deprecated in enum_value_options *)

val enum_value_options_set_uninterpreted_option : enum_value_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in enum_value_options *)

val make_enum_value_descriptor_proto : 
  ?name:string ->
  ?number:int32 ->
  ?options:enum_value_options ->
  unit ->
  enum_value_descriptor_proto
(** [make_enum_value_descriptor_proto … ()] is a builder for type [enum_value_descriptor_proto] *)

val copy_enum_value_descriptor_proto : enum_value_descriptor_proto -> enum_value_descriptor_proto

val enum_value_descriptor_proto_has_name : enum_value_descriptor_proto -> bool
  (** presence of field "name" in [enum_value_descriptor_proto] *)

val enum_value_descriptor_proto_set_name : enum_value_descriptor_proto -> string -> unit
  (** set field name in enum_value_descriptor_proto *)

val enum_value_descriptor_proto_has_number : enum_value_descriptor_proto -> bool
  (** presence of field "number" in [enum_value_descriptor_proto] *)

val enum_value_descriptor_proto_set_number : enum_value_descriptor_proto -> int32 -> unit
  (** set field number in enum_value_descriptor_proto *)

val enum_value_descriptor_proto_has_options : enum_value_descriptor_proto -> bool
  (** presence of field "options" in [enum_value_descriptor_proto] *)

val enum_value_descriptor_proto_set_options : enum_value_descriptor_proto -> enum_value_options -> unit
  (** set field options in enum_value_descriptor_proto *)

val make_enum_options : 
  ?allow_alias:bool ->
  ?deprecated:bool ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  enum_options
(** [make_enum_options … ()] is a builder for type [enum_options] *)

val copy_enum_options : enum_options -> enum_options

val enum_options_has_allow_alias : enum_options -> bool
  (** presence of field "allow_alias" in [enum_options] *)

val enum_options_set_allow_alias : enum_options -> bool -> unit
  (** set field allow_alias in enum_options *)

val enum_options_has_deprecated : enum_options -> bool
  (** presence of field "deprecated" in [enum_options] *)

val enum_options_set_deprecated : enum_options -> bool -> unit
  (** set field deprecated in enum_options *)

val enum_options_set_uninterpreted_option : enum_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in enum_options *)

val make_enum_descriptor_proto_enum_reserved_range : 
  ?start:int32 ->
  ?end_:int32 ->
  unit ->
  enum_descriptor_proto_enum_reserved_range
(** [make_enum_descriptor_proto_enum_reserved_range … ()] is a builder for type [enum_descriptor_proto_enum_reserved_range] *)

val copy_enum_descriptor_proto_enum_reserved_range : enum_descriptor_proto_enum_reserved_range -> enum_descriptor_proto_enum_reserved_range

val enum_descriptor_proto_enum_reserved_range_has_start : enum_descriptor_proto_enum_reserved_range -> bool
  (** presence of field "start" in [enum_descriptor_proto_enum_reserved_range] *)

val enum_descriptor_proto_enum_reserved_range_set_start : enum_descriptor_proto_enum_reserved_range -> int32 -> unit
  (** set field start in enum_descriptor_proto_enum_reserved_range *)

val enum_descriptor_proto_enum_reserved_range_has_end_ : enum_descriptor_proto_enum_reserved_range -> bool
  (** presence of field "end_" in [enum_descriptor_proto_enum_reserved_range] *)

val enum_descriptor_proto_enum_reserved_range_set_end_ : enum_descriptor_proto_enum_reserved_range -> int32 -> unit
  (** set field end_ in enum_descriptor_proto_enum_reserved_range *)

val make_enum_descriptor_proto : 
  ?name:string ->
  ?value:enum_value_descriptor_proto list ->
  ?options:enum_options ->
  ?reserved_range:enum_descriptor_proto_enum_reserved_range list ->
  ?reserved_name:string list ->
  unit ->
  enum_descriptor_proto
(** [make_enum_descriptor_proto … ()] is a builder for type [enum_descriptor_proto] *)

val copy_enum_descriptor_proto : enum_descriptor_proto -> enum_descriptor_proto

val enum_descriptor_proto_has_name : enum_descriptor_proto -> bool
  (** presence of field "name" in [enum_descriptor_proto] *)

val enum_descriptor_proto_set_name : enum_descriptor_proto -> string -> unit
  (** set field name in enum_descriptor_proto *)

val enum_descriptor_proto_set_value : enum_descriptor_proto -> enum_value_descriptor_proto list -> unit
  (** set field value in enum_descriptor_proto *)

val enum_descriptor_proto_has_options : enum_descriptor_proto -> bool
  (** presence of field "options" in [enum_descriptor_proto] *)

val enum_descriptor_proto_set_options : enum_descriptor_proto -> enum_options -> unit
  (** set field options in enum_descriptor_proto *)

val enum_descriptor_proto_set_reserved_range : enum_descriptor_proto -> enum_descriptor_proto_enum_reserved_range list -> unit
  (** set field reserved_range in enum_descriptor_proto *)

val enum_descriptor_proto_set_reserved_name : enum_descriptor_proto -> string list -> unit
  (** set field reserved_name in enum_descriptor_proto *)

val make_extension_range_options : 
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  extension_range_options
(** [make_extension_range_options … ()] is a builder for type [extension_range_options] *)

val copy_extension_range_options : extension_range_options -> extension_range_options

val extension_range_options_set_uninterpreted_option : extension_range_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in extension_range_options *)

val make_descriptor_proto_extension_range : 
  ?start:int32 ->
  ?end_:int32 ->
  ?options:extension_range_options ->
  unit ->
  descriptor_proto_extension_range
(** [make_descriptor_proto_extension_range … ()] is a builder for type [descriptor_proto_extension_range] *)

val copy_descriptor_proto_extension_range : descriptor_proto_extension_range -> descriptor_proto_extension_range

val descriptor_proto_extension_range_has_start : descriptor_proto_extension_range -> bool
  (** presence of field "start" in [descriptor_proto_extension_range] *)

val descriptor_proto_extension_range_set_start : descriptor_proto_extension_range -> int32 -> unit
  (** set field start in descriptor_proto_extension_range *)

val descriptor_proto_extension_range_has_end_ : descriptor_proto_extension_range -> bool
  (** presence of field "end_" in [descriptor_proto_extension_range] *)

val descriptor_proto_extension_range_set_end_ : descriptor_proto_extension_range -> int32 -> unit
  (** set field end_ in descriptor_proto_extension_range *)

val descriptor_proto_extension_range_has_options : descriptor_proto_extension_range -> bool
  (** presence of field "options" in [descriptor_proto_extension_range] *)

val descriptor_proto_extension_range_set_options : descriptor_proto_extension_range -> extension_range_options -> unit
  (** set field options in descriptor_proto_extension_range *)

val make_oneof_options : 
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  oneof_options
(** [make_oneof_options … ()] is a builder for type [oneof_options] *)

val copy_oneof_options : oneof_options -> oneof_options

val oneof_options_set_uninterpreted_option : oneof_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in oneof_options *)

val make_oneof_descriptor_proto : 
  ?name:string ->
  ?options:oneof_options ->
  unit ->
  oneof_descriptor_proto
(** [make_oneof_descriptor_proto … ()] is a builder for type [oneof_descriptor_proto] *)

val copy_oneof_descriptor_proto : oneof_descriptor_proto -> oneof_descriptor_proto

val oneof_descriptor_proto_has_name : oneof_descriptor_proto -> bool
  (** presence of field "name" in [oneof_descriptor_proto] *)

val oneof_descriptor_proto_set_name : oneof_descriptor_proto -> string -> unit
  (** set field name in oneof_descriptor_proto *)

val oneof_descriptor_proto_has_options : oneof_descriptor_proto -> bool
  (** presence of field "options" in [oneof_descriptor_proto] *)

val oneof_descriptor_proto_set_options : oneof_descriptor_proto -> oneof_options -> unit
  (** set field options in oneof_descriptor_proto *)

val make_message_options : 
  ?message_set_wire_format:bool ->
  ?no_standard_descriptor_accessor:bool ->
  ?deprecated:bool ->
  ?map_entry:bool ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  message_options
(** [make_message_options … ()] is a builder for type [message_options] *)

val copy_message_options : message_options -> message_options

val message_options_has_message_set_wire_format : message_options -> bool
  (** presence of field "message_set_wire_format" in [message_options] *)

val message_options_set_message_set_wire_format : message_options -> bool -> unit
  (** set field message_set_wire_format in message_options *)

val message_options_has_no_standard_descriptor_accessor : message_options -> bool
  (** presence of field "no_standard_descriptor_accessor" in [message_options] *)

val message_options_set_no_standard_descriptor_accessor : message_options -> bool -> unit
  (** set field no_standard_descriptor_accessor in message_options *)

val message_options_has_deprecated : message_options -> bool
  (** presence of field "deprecated" in [message_options] *)

val message_options_set_deprecated : message_options -> bool -> unit
  (** set field deprecated in message_options *)

val message_options_has_map_entry : message_options -> bool
  (** presence of field "map_entry" in [message_options] *)

val message_options_set_map_entry : message_options -> bool -> unit
  (** set field map_entry in message_options *)

val message_options_set_uninterpreted_option : message_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in message_options *)

val make_descriptor_proto_reserved_range : 
  ?start:int32 ->
  ?end_:int32 ->
  unit ->
  descriptor_proto_reserved_range
(** [make_descriptor_proto_reserved_range … ()] is a builder for type [descriptor_proto_reserved_range] *)

val copy_descriptor_proto_reserved_range : descriptor_proto_reserved_range -> descriptor_proto_reserved_range

val descriptor_proto_reserved_range_has_start : descriptor_proto_reserved_range -> bool
  (** presence of field "start" in [descriptor_proto_reserved_range] *)

val descriptor_proto_reserved_range_set_start : descriptor_proto_reserved_range -> int32 -> unit
  (** set field start in descriptor_proto_reserved_range *)

val descriptor_proto_reserved_range_has_end_ : descriptor_proto_reserved_range -> bool
  (** presence of field "end_" in [descriptor_proto_reserved_range] *)

val descriptor_proto_reserved_range_set_end_ : descriptor_proto_reserved_range -> int32 -> unit
  (** set field end_ in descriptor_proto_reserved_range *)

val make_descriptor_proto : 
  ?name:string ->
  ?field:field_descriptor_proto list ->
  ?extension:field_descriptor_proto list ->
  ?nested_type:descriptor_proto list ->
  ?enum_type:enum_descriptor_proto list ->
  ?extension_range:descriptor_proto_extension_range list ->
  ?oneof_decl:oneof_descriptor_proto list ->
  ?options:message_options ->
  ?reserved_range:descriptor_proto_reserved_range list ->
  ?reserved_name:string list ->
  unit ->
  descriptor_proto
(** [make_descriptor_proto … ()] is a builder for type [descriptor_proto] *)

val copy_descriptor_proto : descriptor_proto -> descriptor_proto

val descriptor_proto_has_name : descriptor_proto -> bool
  (** presence of field "name" in [descriptor_proto] *)

val descriptor_proto_set_name : descriptor_proto -> string -> unit
  (** set field name in descriptor_proto *)

val descriptor_proto_set_field : descriptor_proto -> field_descriptor_proto list -> unit
  (** set field field in descriptor_proto *)

val descriptor_proto_set_extension : descriptor_proto -> field_descriptor_proto list -> unit
  (** set field extension in descriptor_proto *)

val descriptor_proto_set_nested_type : descriptor_proto -> descriptor_proto list -> unit
  (** set field nested_type in descriptor_proto *)

val descriptor_proto_set_enum_type : descriptor_proto -> enum_descriptor_proto list -> unit
  (** set field enum_type in descriptor_proto *)

val descriptor_proto_set_extension_range : descriptor_proto -> descriptor_proto_extension_range list -> unit
  (** set field extension_range in descriptor_proto *)

val descriptor_proto_set_oneof_decl : descriptor_proto -> oneof_descriptor_proto list -> unit
  (** set field oneof_decl in descriptor_proto *)

val descriptor_proto_has_options : descriptor_proto -> bool
  (** presence of field "options" in [descriptor_proto] *)

val descriptor_proto_set_options : descriptor_proto -> message_options -> unit
  (** set field options in descriptor_proto *)

val descriptor_proto_set_reserved_range : descriptor_proto -> descriptor_proto_reserved_range list -> unit
  (** set field reserved_range in descriptor_proto *)

val descriptor_proto_set_reserved_name : descriptor_proto -> string list -> unit
  (** set field reserved_name in descriptor_proto *)

val make_method_options : 
  ?deprecated:bool ->
  ?idempotency_level:method_options_idempotency_level ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  method_options
(** [make_method_options … ()] is a builder for type [method_options] *)

val copy_method_options : method_options -> method_options

val method_options_has_deprecated : method_options -> bool
  (** presence of field "deprecated" in [method_options] *)

val method_options_set_deprecated : method_options -> bool -> unit
  (** set field deprecated in method_options *)

val method_options_has_idempotency_level : method_options -> bool
  (** presence of field "idempotency_level" in [method_options] *)

val method_options_set_idempotency_level : method_options -> method_options_idempotency_level -> unit
  (** set field idempotency_level in method_options *)

val method_options_set_uninterpreted_option : method_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in method_options *)

val make_method_descriptor_proto : 
  ?name:string ->
  ?input_type:string ->
  ?output_type:string ->
  ?options:method_options ->
  ?client_streaming:bool ->
  ?server_streaming:bool ->
  unit ->
  method_descriptor_proto
(** [make_method_descriptor_proto … ()] is a builder for type [method_descriptor_proto] *)

val copy_method_descriptor_proto : method_descriptor_proto -> method_descriptor_proto

val method_descriptor_proto_has_name : method_descriptor_proto -> bool
  (** presence of field "name" in [method_descriptor_proto] *)

val method_descriptor_proto_set_name : method_descriptor_proto -> string -> unit
  (** set field name in method_descriptor_proto *)

val method_descriptor_proto_has_input_type : method_descriptor_proto -> bool
  (** presence of field "input_type" in [method_descriptor_proto] *)

val method_descriptor_proto_set_input_type : method_descriptor_proto -> string -> unit
  (** set field input_type in method_descriptor_proto *)

val method_descriptor_proto_has_output_type : method_descriptor_proto -> bool
  (** presence of field "output_type" in [method_descriptor_proto] *)

val method_descriptor_proto_set_output_type : method_descriptor_proto -> string -> unit
  (** set field output_type in method_descriptor_proto *)

val method_descriptor_proto_has_options : method_descriptor_proto -> bool
  (** presence of field "options" in [method_descriptor_proto] *)

val method_descriptor_proto_set_options : method_descriptor_proto -> method_options -> unit
  (** set field options in method_descriptor_proto *)

val method_descriptor_proto_has_client_streaming : method_descriptor_proto -> bool
  (** presence of field "client_streaming" in [method_descriptor_proto] *)

val method_descriptor_proto_set_client_streaming : method_descriptor_proto -> bool -> unit
  (** set field client_streaming in method_descriptor_proto *)

val method_descriptor_proto_has_server_streaming : method_descriptor_proto -> bool
  (** presence of field "server_streaming" in [method_descriptor_proto] *)

val method_descriptor_proto_set_server_streaming : method_descriptor_proto -> bool -> unit
  (** set field server_streaming in method_descriptor_proto *)

val make_service_options : 
  ?deprecated:bool ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  service_options
(** [make_service_options … ()] is a builder for type [service_options] *)

val copy_service_options : service_options -> service_options

val service_options_has_deprecated : service_options -> bool
  (** presence of field "deprecated" in [service_options] *)

val service_options_set_deprecated : service_options -> bool -> unit
  (** set field deprecated in service_options *)

val service_options_set_uninterpreted_option : service_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in service_options *)

val make_service_descriptor_proto : 
  ?name:string ->
  ?method_:method_descriptor_proto list ->
  ?options:service_options ->
  unit ->
  service_descriptor_proto
(** [make_service_descriptor_proto … ()] is a builder for type [service_descriptor_proto] *)

val copy_service_descriptor_proto : service_descriptor_proto -> service_descriptor_proto

val service_descriptor_proto_has_name : service_descriptor_proto -> bool
  (** presence of field "name" in [service_descriptor_proto] *)

val service_descriptor_proto_set_name : service_descriptor_proto -> string -> unit
  (** set field name in service_descriptor_proto *)

val service_descriptor_proto_set_method_ : service_descriptor_proto -> method_descriptor_proto list -> unit
  (** set field method_ in service_descriptor_proto *)

val service_descriptor_proto_has_options : service_descriptor_proto -> bool
  (** presence of field "options" in [service_descriptor_proto] *)

val service_descriptor_proto_set_options : service_descriptor_proto -> service_options -> unit
  (** set field options in service_descriptor_proto *)

val make_file_options : 
  ?java_package:string ->
  ?java_outer_classname:string ->
  ?java_multiple_files:bool ->
  ?java_generate_equals_and_hash:bool ->
  ?java_string_check_utf8:bool ->
  ?optimize_for:file_options_optimize_mode ->
  ?go_package:string ->
  ?cc_generic_services:bool ->
  ?java_generic_services:bool ->
  ?py_generic_services:bool ->
  ?php_generic_services:bool ->
  ?deprecated:bool ->
  ?cc_enable_arenas:bool ->
  ?objc_class_prefix:string ->
  ?csharp_namespace:string ->
  ?swift_prefix:string ->
  ?php_class_prefix:string ->
  ?php_namespace:string ->
  ?php_metadata_namespace:string ->
  ?ruby_package:string ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  file_options
(** [make_file_options … ()] is a builder for type [file_options] *)

val copy_file_options : file_options -> file_options

val file_options_has_java_package : file_options -> bool
  (** presence of field "java_package" in [file_options] *)

val file_options_set_java_package : file_options -> string -> unit
  (** set field java_package in file_options *)

val file_options_has_java_outer_classname : file_options -> bool
  (** presence of field "java_outer_classname" in [file_options] *)

val file_options_set_java_outer_classname : file_options -> string -> unit
  (** set field java_outer_classname in file_options *)

val file_options_has_java_multiple_files : file_options -> bool
  (** presence of field "java_multiple_files" in [file_options] *)

val file_options_set_java_multiple_files : file_options -> bool -> unit
  (** set field java_multiple_files in file_options *)

val file_options_has_java_generate_equals_and_hash : file_options -> bool
  (** presence of field "java_generate_equals_and_hash" in [file_options] *)

val file_options_set_java_generate_equals_and_hash : file_options -> bool -> unit
  (** set field java_generate_equals_and_hash in file_options *)

val file_options_has_java_string_check_utf8 : file_options -> bool
  (** presence of field "java_string_check_utf8" in [file_options] *)

val file_options_set_java_string_check_utf8 : file_options -> bool -> unit
  (** set field java_string_check_utf8 in file_options *)

val file_options_has_optimize_for : file_options -> bool
  (** presence of field "optimize_for" in [file_options] *)

val file_options_set_optimize_for : file_options -> file_options_optimize_mode -> unit
  (** set field optimize_for in file_options *)

val file_options_has_go_package : file_options -> bool
  (** presence of field "go_package" in [file_options] *)

val file_options_set_go_package : file_options -> string -> unit
  (** set field go_package in file_options *)

val file_options_has_cc_generic_services : file_options -> bool
  (** presence of field "cc_generic_services" in [file_options] *)

val file_options_set_cc_generic_services : file_options -> bool -> unit
  (** set field cc_generic_services in file_options *)

val file_options_has_java_generic_services : file_options -> bool
  (** presence of field "java_generic_services" in [file_options] *)

val file_options_set_java_generic_services : file_options -> bool -> unit
  (** set field java_generic_services in file_options *)

val file_options_has_py_generic_services : file_options -> bool
  (** presence of field "py_generic_services" in [file_options] *)

val file_options_set_py_generic_services : file_options -> bool -> unit
  (** set field py_generic_services in file_options *)

val file_options_has_php_generic_services : file_options -> bool
  (** presence of field "php_generic_services" in [file_options] *)

val file_options_set_php_generic_services : file_options -> bool -> unit
  (** set field php_generic_services in file_options *)

val file_options_has_deprecated : file_options -> bool
  (** presence of field "deprecated" in [file_options] *)

val file_options_set_deprecated : file_options -> bool -> unit
  (** set field deprecated in file_options *)

val file_options_has_cc_enable_arenas : file_options -> bool
  (** presence of field "cc_enable_arenas" in [file_options] *)

val file_options_set_cc_enable_arenas : file_options -> bool -> unit
  (** set field cc_enable_arenas in file_options *)

val file_options_has_objc_class_prefix : file_options -> bool
  (** presence of field "objc_class_prefix" in [file_options] *)

val file_options_set_objc_class_prefix : file_options -> string -> unit
  (** set field objc_class_prefix in file_options *)

val file_options_has_csharp_namespace : file_options -> bool
  (** presence of field "csharp_namespace" in [file_options] *)

val file_options_set_csharp_namespace : file_options -> string -> unit
  (** set field csharp_namespace in file_options *)

val file_options_has_swift_prefix : file_options -> bool
  (** presence of field "swift_prefix" in [file_options] *)

val file_options_set_swift_prefix : file_options -> string -> unit
  (** set field swift_prefix in file_options *)

val file_options_has_php_class_prefix : file_options -> bool
  (** presence of field "php_class_prefix" in [file_options] *)

val file_options_set_php_class_prefix : file_options -> string -> unit
  (** set field php_class_prefix in file_options *)

val file_options_has_php_namespace : file_options -> bool
  (** presence of field "php_namespace" in [file_options] *)

val file_options_set_php_namespace : file_options -> string -> unit
  (** set field php_namespace in file_options *)

val file_options_has_php_metadata_namespace : file_options -> bool
  (** presence of field "php_metadata_namespace" in [file_options] *)

val file_options_set_php_metadata_namespace : file_options -> string -> unit
  (** set field php_metadata_namespace in file_options *)

val file_options_has_ruby_package : file_options -> bool
  (** presence of field "ruby_package" in [file_options] *)

val file_options_set_ruby_package : file_options -> string -> unit
  (** set field ruby_package in file_options *)

val file_options_set_uninterpreted_option : file_options -> uninterpreted_option list -> unit
  (** set field uninterpreted_option in file_options *)

val make_source_code_info_location : 
  ?path:int32 list ->
  ?span:int32 list ->
  ?leading_comments:string ->
  ?trailing_comments:string ->
  ?leading_detached_comments:string list ->
  unit ->
  source_code_info_location
(** [make_source_code_info_location … ()] is a builder for type [source_code_info_location] *)

val copy_source_code_info_location : source_code_info_location -> source_code_info_location

val source_code_info_location_set_path : source_code_info_location -> int32 list -> unit
  (** set field path in source_code_info_location *)

val source_code_info_location_set_span : source_code_info_location -> int32 list -> unit
  (** set field span in source_code_info_location *)

val source_code_info_location_has_leading_comments : source_code_info_location -> bool
  (** presence of field "leading_comments" in [source_code_info_location] *)

val source_code_info_location_set_leading_comments : source_code_info_location -> string -> unit
  (** set field leading_comments in source_code_info_location *)

val source_code_info_location_has_trailing_comments : source_code_info_location -> bool
  (** presence of field "trailing_comments" in [source_code_info_location] *)

val source_code_info_location_set_trailing_comments : source_code_info_location -> string -> unit
  (** set field trailing_comments in source_code_info_location *)

val source_code_info_location_set_leading_detached_comments : source_code_info_location -> string list -> unit
  (** set field leading_detached_comments in source_code_info_location *)

val make_source_code_info : 
  ?location:source_code_info_location list ->
  unit ->
  source_code_info
(** [make_source_code_info … ()] is a builder for type [source_code_info] *)

val copy_source_code_info : source_code_info -> source_code_info

val source_code_info_set_location : source_code_info -> source_code_info_location list -> unit
  (** set field location in source_code_info *)

val make_file_descriptor_proto : 
  ?name:string ->
  ?package:string ->
  ?dependency:string list ->
  ?public_dependency:int32 list ->
  ?weak_dependency:int32 list ->
  ?message_type:descriptor_proto list ->
  ?enum_type:enum_descriptor_proto list ->
  ?service:service_descriptor_proto list ->
  ?extension:field_descriptor_proto list ->
  ?options:file_options ->
  ?source_code_info:source_code_info ->
  ?syntax:string ->
  unit ->
  file_descriptor_proto
(** [make_file_descriptor_proto … ()] is a builder for type [file_descriptor_proto] *)

val copy_file_descriptor_proto : file_descriptor_proto -> file_descriptor_proto

val file_descriptor_proto_has_name : file_descriptor_proto -> bool
  (** presence of field "name" in [file_descriptor_proto] *)

val file_descriptor_proto_set_name : file_descriptor_proto -> string -> unit
  (** set field name in file_descriptor_proto *)

val file_descriptor_proto_has_package : file_descriptor_proto -> bool
  (** presence of field "package" in [file_descriptor_proto] *)

val file_descriptor_proto_set_package : file_descriptor_proto -> string -> unit
  (** set field package in file_descriptor_proto *)

val file_descriptor_proto_set_dependency : file_descriptor_proto -> string list -> unit
  (** set field dependency in file_descriptor_proto *)

val file_descriptor_proto_set_public_dependency : file_descriptor_proto -> int32 list -> unit
  (** set field public_dependency in file_descriptor_proto *)

val file_descriptor_proto_set_weak_dependency : file_descriptor_proto -> int32 list -> unit
  (** set field weak_dependency in file_descriptor_proto *)

val file_descriptor_proto_set_message_type : file_descriptor_proto -> descriptor_proto list -> unit
  (** set field message_type in file_descriptor_proto *)

val file_descriptor_proto_set_enum_type : file_descriptor_proto -> enum_descriptor_proto list -> unit
  (** set field enum_type in file_descriptor_proto *)

val file_descriptor_proto_set_service : file_descriptor_proto -> service_descriptor_proto list -> unit
  (** set field service in file_descriptor_proto *)

val file_descriptor_proto_set_extension : file_descriptor_proto -> field_descriptor_proto list -> unit
  (** set field extension in file_descriptor_proto *)

val file_descriptor_proto_has_options : file_descriptor_proto -> bool
  (** presence of field "options" in [file_descriptor_proto] *)

val file_descriptor_proto_set_options : file_descriptor_proto -> file_options -> unit
  (** set field options in file_descriptor_proto *)

val file_descriptor_proto_has_source_code_info : file_descriptor_proto -> bool
  (** presence of field "source_code_info" in [file_descriptor_proto] *)

val file_descriptor_proto_set_source_code_info : file_descriptor_proto -> source_code_info -> unit
  (** set field source_code_info in file_descriptor_proto *)

val file_descriptor_proto_has_syntax : file_descriptor_proto -> bool
  (** presence of field "syntax" in [file_descriptor_proto] *)

val file_descriptor_proto_set_syntax : file_descriptor_proto -> string -> unit
  (** set field syntax in file_descriptor_proto *)

val make_file_descriptor_set : 
  ?file:file_descriptor_proto list ->
  unit ->
  file_descriptor_set
(** [make_file_descriptor_set … ()] is a builder for type [file_descriptor_set] *)

val copy_file_descriptor_set : file_descriptor_set -> file_descriptor_set

val file_descriptor_set_set_file : file_descriptor_set -> file_descriptor_proto list -> unit
  (** set field file in file_descriptor_set *)

val make_generated_code_info_annotation : 
  ?path:int32 list ->
  ?source_file:string ->
  ?begin_:int32 ->
  ?end_:int32 ->
  unit ->
  generated_code_info_annotation
(** [make_generated_code_info_annotation … ()] is a builder for type [generated_code_info_annotation] *)

val copy_generated_code_info_annotation : generated_code_info_annotation -> generated_code_info_annotation

val generated_code_info_annotation_set_path : generated_code_info_annotation -> int32 list -> unit
  (** set field path in generated_code_info_annotation *)

val generated_code_info_annotation_has_source_file : generated_code_info_annotation -> bool
  (** presence of field "source_file" in [generated_code_info_annotation] *)

val generated_code_info_annotation_set_source_file : generated_code_info_annotation -> string -> unit
  (** set field source_file in generated_code_info_annotation *)

val generated_code_info_annotation_has_begin_ : generated_code_info_annotation -> bool
  (** presence of field "begin_" in [generated_code_info_annotation] *)

val generated_code_info_annotation_set_begin_ : generated_code_info_annotation -> int32 -> unit
  (** set field begin_ in generated_code_info_annotation *)

val generated_code_info_annotation_has_end_ : generated_code_info_annotation -> bool
  (** presence of field "end_" in [generated_code_info_annotation] *)

val generated_code_info_annotation_set_end_ : generated_code_info_annotation -> int32 -> unit
  (** set field end_ in generated_code_info_annotation *)

val make_generated_code_info : 
  ?annotation:generated_code_info_annotation list ->
  unit ->
  generated_code_info
(** [make_generated_code_info … ()] is a builder for type [generated_code_info] *)

val copy_generated_code_info : generated_code_info -> generated_code_info

val generated_code_info_set_annotation : generated_code_info -> generated_code_info_annotation list -> unit
  (** set field annotation in generated_code_info *)


(** {2 Protobuf Encoding} *)

val encode_pb_field_descriptor_proto_label : field_descriptor_proto_label -> Pbrt.Encoder.t -> unit
(** [encode_pb_field_descriptor_proto_label v encoder] encodes [v] with the given [encoder] *)

val encode_pb_field_descriptor_proto_type : field_descriptor_proto_type -> Pbrt.Encoder.t -> unit
(** [encode_pb_field_descriptor_proto_type v encoder] encodes [v] with the given [encoder] *)

val encode_pb_field_options_ctype : field_options_ctype -> Pbrt.Encoder.t -> unit
(** [encode_pb_field_options_ctype v encoder] encodes [v] with the given [encoder] *)

val encode_pb_field_options_jstype : field_options_jstype -> Pbrt.Encoder.t -> unit
(** [encode_pb_field_options_jstype v encoder] encodes [v] with the given [encoder] *)

val encode_pb_uninterpreted_option_name_part : uninterpreted_option_name_part -> Pbrt.Encoder.t -> unit
(** [encode_pb_uninterpreted_option_name_part v encoder] encodes [v] with the given [encoder] *)

val encode_pb_uninterpreted_option : uninterpreted_option -> Pbrt.Encoder.t -> unit
(** [encode_pb_uninterpreted_option v encoder] encodes [v] with the given [encoder] *)

val encode_pb_field_options : field_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_field_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_field_descriptor_proto : field_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_field_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_enum_value_options : enum_value_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_enum_value_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_enum_value_descriptor_proto : enum_value_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_enum_value_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_enum_options : enum_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_enum_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_enum_descriptor_proto_enum_reserved_range : enum_descriptor_proto_enum_reserved_range -> Pbrt.Encoder.t -> unit
(** [encode_pb_enum_descriptor_proto_enum_reserved_range v encoder] encodes [v] with the given [encoder] *)

val encode_pb_enum_descriptor_proto : enum_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_enum_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_extension_range_options : extension_range_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_extension_range_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_descriptor_proto_extension_range : descriptor_proto_extension_range -> Pbrt.Encoder.t -> unit
(** [encode_pb_descriptor_proto_extension_range v encoder] encodes [v] with the given [encoder] *)

val encode_pb_oneof_options : oneof_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_oneof_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_oneof_descriptor_proto : oneof_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_oneof_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_message_options : message_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_message_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_descriptor_proto_reserved_range : descriptor_proto_reserved_range -> Pbrt.Encoder.t -> unit
(** [encode_pb_descriptor_proto_reserved_range v encoder] encodes [v] with the given [encoder] *)

val encode_pb_descriptor_proto : descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_method_options_idempotency_level : method_options_idempotency_level -> Pbrt.Encoder.t -> unit
(** [encode_pb_method_options_idempotency_level v encoder] encodes [v] with the given [encoder] *)

val encode_pb_method_options : method_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_method_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_method_descriptor_proto : method_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_method_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_service_options : service_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_service_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_service_descriptor_proto : service_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_service_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_file_options_optimize_mode : file_options_optimize_mode -> Pbrt.Encoder.t -> unit
(** [encode_pb_file_options_optimize_mode v encoder] encodes [v] with the given [encoder] *)

val encode_pb_file_options : file_options -> Pbrt.Encoder.t -> unit
(** [encode_pb_file_options v encoder] encodes [v] with the given [encoder] *)

val encode_pb_source_code_info_location : source_code_info_location -> Pbrt.Encoder.t -> unit
(** [encode_pb_source_code_info_location v encoder] encodes [v] with the given [encoder] *)

val encode_pb_source_code_info : source_code_info -> Pbrt.Encoder.t -> unit
(** [encode_pb_source_code_info v encoder] encodes [v] with the given [encoder] *)

val encode_pb_file_descriptor_proto : file_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_pb_file_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_pb_file_descriptor_set : file_descriptor_set -> Pbrt.Encoder.t -> unit
(** [encode_pb_file_descriptor_set v encoder] encodes [v] with the given [encoder] *)

val encode_pb_generated_code_info_annotation : generated_code_info_annotation -> Pbrt.Encoder.t -> unit
(** [encode_pb_generated_code_info_annotation v encoder] encodes [v] with the given [encoder] *)

val encode_pb_generated_code_info : generated_code_info -> Pbrt.Encoder.t -> unit
(** [encode_pb_generated_code_info v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_field_descriptor_proto_label : Pbrt.Decoder.t -> field_descriptor_proto_label
(** [decode_pb_field_descriptor_proto_label decoder] decodes a [field_descriptor_proto_label] binary value from [decoder] *)

val decode_pb_field_descriptor_proto_type : Pbrt.Decoder.t -> field_descriptor_proto_type
(** [decode_pb_field_descriptor_proto_type decoder] decodes a [field_descriptor_proto_type] binary value from [decoder] *)

val decode_pb_field_options_ctype : Pbrt.Decoder.t -> field_options_ctype
(** [decode_pb_field_options_ctype decoder] decodes a [field_options_ctype] binary value from [decoder] *)

val decode_pb_field_options_jstype : Pbrt.Decoder.t -> field_options_jstype
(** [decode_pb_field_options_jstype decoder] decodes a [field_options_jstype] binary value from [decoder] *)

val decode_pb_uninterpreted_option_name_part : Pbrt.Decoder.t -> uninterpreted_option_name_part
(** [decode_pb_uninterpreted_option_name_part decoder] decodes a [uninterpreted_option_name_part] binary value from [decoder] *)

val decode_pb_uninterpreted_option : Pbrt.Decoder.t -> uninterpreted_option
(** [decode_pb_uninterpreted_option decoder] decodes a [uninterpreted_option] binary value from [decoder] *)

val decode_pb_field_options : Pbrt.Decoder.t -> field_options
(** [decode_pb_field_options decoder] decodes a [field_options] binary value from [decoder] *)

val decode_pb_field_descriptor_proto : Pbrt.Decoder.t -> field_descriptor_proto
(** [decode_pb_field_descriptor_proto decoder] decodes a [field_descriptor_proto] binary value from [decoder] *)

val decode_pb_enum_value_options : Pbrt.Decoder.t -> enum_value_options
(** [decode_pb_enum_value_options decoder] decodes a [enum_value_options] binary value from [decoder] *)

val decode_pb_enum_value_descriptor_proto : Pbrt.Decoder.t -> enum_value_descriptor_proto
(** [decode_pb_enum_value_descriptor_proto decoder] decodes a [enum_value_descriptor_proto] binary value from [decoder] *)

val decode_pb_enum_options : Pbrt.Decoder.t -> enum_options
(** [decode_pb_enum_options decoder] decodes a [enum_options] binary value from [decoder] *)

val decode_pb_enum_descriptor_proto_enum_reserved_range : Pbrt.Decoder.t -> enum_descriptor_proto_enum_reserved_range
(** [decode_pb_enum_descriptor_proto_enum_reserved_range decoder] decodes a [enum_descriptor_proto_enum_reserved_range] binary value from [decoder] *)

val decode_pb_enum_descriptor_proto : Pbrt.Decoder.t -> enum_descriptor_proto
(** [decode_pb_enum_descriptor_proto decoder] decodes a [enum_descriptor_proto] binary value from [decoder] *)

val decode_pb_extension_range_options : Pbrt.Decoder.t -> extension_range_options
(** [decode_pb_extension_range_options decoder] decodes a [extension_range_options] binary value from [decoder] *)

val decode_pb_descriptor_proto_extension_range : Pbrt.Decoder.t -> descriptor_proto_extension_range
(** [decode_pb_descriptor_proto_extension_range decoder] decodes a [descriptor_proto_extension_range] binary value from [decoder] *)

val decode_pb_oneof_options : Pbrt.Decoder.t -> oneof_options
(** [decode_pb_oneof_options decoder] decodes a [oneof_options] binary value from [decoder] *)

val decode_pb_oneof_descriptor_proto : Pbrt.Decoder.t -> oneof_descriptor_proto
(** [decode_pb_oneof_descriptor_proto decoder] decodes a [oneof_descriptor_proto] binary value from [decoder] *)

val decode_pb_message_options : Pbrt.Decoder.t -> message_options
(** [decode_pb_message_options decoder] decodes a [message_options] binary value from [decoder] *)

val decode_pb_descriptor_proto_reserved_range : Pbrt.Decoder.t -> descriptor_proto_reserved_range
(** [decode_pb_descriptor_proto_reserved_range decoder] decodes a [descriptor_proto_reserved_range] binary value from [decoder] *)

val decode_pb_descriptor_proto : Pbrt.Decoder.t -> descriptor_proto
(** [decode_pb_descriptor_proto decoder] decodes a [descriptor_proto] binary value from [decoder] *)

val decode_pb_method_options_idempotency_level : Pbrt.Decoder.t -> method_options_idempotency_level
(** [decode_pb_method_options_idempotency_level decoder] decodes a [method_options_idempotency_level] binary value from [decoder] *)

val decode_pb_method_options : Pbrt.Decoder.t -> method_options
(** [decode_pb_method_options decoder] decodes a [method_options] binary value from [decoder] *)

val decode_pb_method_descriptor_proto : Pbrt.Decoder.t -> method_descriptor_proto
(** [decode_pb_method_descriptor_proto decoder] decodes a [method_descriptor_proto] binary value from [decoder] *)

val decode_pb_service_options : Pbrt.Decoder.t -> service_options
(** [decode_pb_service_options decoder] decodes a [service_options] binary value from [decoder] *)

val decode_pb_service_descriptor_proto : Pbrt.Decoder.t -> service_descriptor_proto
(** [decode_pb_service_descriptor_proto decoder] decodes a [service_descriptor_proto] binary value from [decoder] *)

val decode_pb_file_options_optimize_mode : Pbrt.Decoder.t -> file_options_optimize_mode
(** [decode_pb_file_options_optimize_mode decoder] decodes a [file_options_optimize_mode] binary value from [decoder] *)

val decode_pb_file_options : Pbrt.Decoder.t -> file_options
(** [decode_pb_file_options decoder] decodes a [file_options] binary value from [decoder] *)

val decode_pb_source_code_info_location : Pbrt.Decoder.t -> source_code_info_location
(** [decode_pb_source_code_info_location decoder] decodes a [source_code_info_location] binary value from [decoder] *)

val decode_pb_source_code_info : Pbrt.Decoder.t -> source_code_info
(** [decode_pb_source_code_info decoder] decodes a [source_code_info] binary value from [decoder] *)

val decode_pb_file_descriptor_proto : Pbrt.Decoder.t -> file_descriptor_proto
(** [decode_pb_file_descriptor_proto decoder] decodes a [file_descriptor_proto] binary value from [decoder] *)

val decode_pb_file_descriptor_set : Pbrt.Decoder.t -> file_descriptor_set
(** [decode_pb_file_descriptor_set decoder] decodes a [file_descriptor_set] binary value from [decoder] *)

val decode_pb_generated_code_info_annotation : Pbrt.Decoder.t -> generated_code_info_annotation
(** [decode_pb_generated_code_info_annotation decoder] decodes a [generated_code_info_annotation] binary value from [decoder] *)

val decode_pb_generated_code_info : Pbrt.Decoder.t -> generated_code_info
(** [decode_pb_generated_code_info decoder] decodes a [generated_code_info] binary value from [decoder] *)


(** {2 Protobuf YoJson Encoding} *)

val encode_json_field_descriptor_proto_label : field_descriptor_proto_label -> Yojson.Basic.t
(** [encode_json_field_descriptor_proto_label v encoder] encodes [v] to to json *)

val encode_json_field_descriptor_proto_type : field_descriptor_proto_type -> Yojson.Basic.t
(** [encode_json_field_descriptor_proto_type v encoder] encodes [v] to to json *)

val encode_json_field_options_ctype : field_options_ctype -> Yojson.Basic.t
(** [encode_json_field_options_ctype v encoder] encodes [v] to to json *)

val encode_json_field_options_jstype : field_options_jstype -> Yojson.Basic.t
(** [encode_json_field_options_jstype v encoder] encodes [v] to to json *)

val encode_json_uninterpreted_option_name_part : uninterpreted_option_name_part -> Yojson.Basic.t
(** [encode_json_uninterpreted_option_name_part v encoder] encodes [v] to to json *)

val encode_json_uninterpreted_option : uninterpreted_option -> Yojson.Basic.t
(** [encode_json_uninterpreted_option v encoder] encodes [v] to to json *)

val encode_json_field_options : field_options -> Yojson.Basic.t
(** [encode_json_field_options v encoder] encodes [v] to to json *)

val encode_json_field_descriptor_proto : field_descriptor_proto -> Yojson.Basic.t
(** [encode_json_field_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_enum_value_options : enum_value_options -> Yojson.Basic.t
(** [encode_json_enum_value_options v encoder] encodes [v] to to json *)

val encode_json_enum_value_descriptor_proto : enum_value_descriptor_proto -> Yojson.Basic.t
(** [encode_json_enum_value_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_enum_options : enum_options -> Yojson.Basic.t
(** [encode_json_enum_options v encoder] encodes [v] to to json *)

val encode_json_enum_descriptor_proto_enum_reserved_range : enum_descriptor_proto_enum_reserved_range -> Yojson.Basic.t
(** [encode_json_enum_descriptor_proto_enum_reserved_range v encoder] encodes [v] to to json *)

val encode_json_enum_descriptor_proto : enum_descriptor_proto -> Yojson.Basic.t
(** [encode_json_enum_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_extension_range_options : extension_range_options -> Yojson.Basic.t
(** [encode_json_extension_range_options v encoder] encodes [v] to to json *)

val encode_json_descriptor_proto_extension_range : descriptor_proto_extension_range -> Yojson.Basic.t
(** [encode_json_descriptor_proto_extension_range v encoder] encodes [v] to to json *)

val encode_json_oneof_options : oneof_options -> Yojson.Basic.t
(** [encode_json_oneof_options v encoder] encodes [v] to to json *)

val encode_json_oneof_descriptor_proto : oneof_descriptor_proto -> Yojson.Basic.t
(** [encode_json_oneof_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_message_options : message_options -> Yojson.Basic.t
(** [encode_json_message_options v encoder] encodes [v] to to json *)

val encode_json_descriptor_proto_reserved_range : descriptor_proto_reserved_range -> Yojson.Basic.t
(** [encode_json_descriptor_proto_reserved_range v encoder] encodes [v] to to json *)

val encode_json_descriptor_proto : descriptor_proto -> Yojson.Basic.t
(** [encode_json_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_method_options_idempotency_level : method_options_idempotency_level -> Yojson.Basic.t
(** [encode_json_method_options_idempotency_level v encoder] encodes [v] to to json *)

val encode_json_method_options : method_options -> Yojson.Basic.t
(** [encode_json_method_options v encoder] encodes [v] to to json *)

val encode_json_method_descriptor_proto : method_descriptor_proto -> Yojson.Basic.t
(** [encode_json_method_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_service_options : service_options -> Yojson.Basic.t
(** [encode_json_service_options v encoder] encodes [v] to to json *)

val encode_json_service_descriptor_proto : service_descriptor_proto -> Yojson.Basic.t
(** [encode_json_service_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_file_options_optimize_mode : file_options_optimize_mode -> Yojson.Basic.t
(** [encode_json_file_options_optimize_mode v encoder] encodes [v] to to json *)

val encode_json_file_options : file_options -> Yojson.Basic.t
(** [encode_json_file_options v encoder] encodes [v] to to json *)

val encode_json_source_code_info_location : source_code_info_location -> Yojson.Basic.t
(** [encode_json_source_code_info_location v encoder] encodes [v] to to json *)

val encode_json_source_code_info : source_code_info -> Yojson.Basic.t
(** [encode_json_source_code_info v encoder] encodes [v] to to json *)

val encode_json_file_descriptor_proto : file_descriptor_proto -> Yojson.Basic.t
(** [encode_json_file_descriptor_proto v encoder] encodes [v] to to json *)

val encode_json_file_descriptor_set : file_descriptor_set -> Yojson.Basic.t
(** [encode_json_file_descriptor_set v encoder] encodes [v] to to json *)

val encode_json_generated_code_info_annotation : generated_code_info_annotation -> Yojson.Basic.t
(** [encode_json_generated_code_info_annotation v encoder] encodes [v] to to json *)

val encode_json_generated_code_info : generated_code_info -> Yojson.Basic.t
(** [encode_json_generated_code_info v encoder] encodes [v] to to json *)


(** {2 JSON Decoding} *)

val decode_json_field_descriptor_proto_label : Yojson.Basic.t -> field_descriptor_proto_label
(** [decode_json_field_descriptor_proto_label decoder] decodes a [field_descriptor_proto_label] value from [decoder] *)

val decode_json_field_descriptor_proto_type : Yojson.Basic.t -> field_descriptor_proto_type
(** [decode_json_field_descriptor_proto_type decoder] decodes a [field_descriptor_proto_type] value from [decoder] *)

val decode_json_field_options_ctype : Yojson.Basic.t -> field_options_ctype
(** [decode_json_field_options_ctype decoder] decodes a [field_options_ctype] value from [decoder] *)

val decode_json_field_options_jstype : Yojson.Basic.t -> field_options_jstype
(** [decode_json_field_options_jstype decoder] decodes a [field_options_jstype] value from [decoder] *)

val decode_json_uninterpreted_option_name_part : Yojson.Basic.t -> uninterpreted_option_name_part
(** [decode_json_uninterpreted_option_name_part decoder] decodes a [uninterpreted_option_name_part] value from [decoder] *)

val decode_json_uninterpreted_option : Yojson.Basic.t -> uninterpreted_option
(** [decode_json_uninterpreted_option decoder] decodes a [uninterpreted_option] value from [decoder] *)

val decode_json_field_options : Yojson.Basic.t -> field_options
(** [decode_json_field_options decoder] decodes a [field_options] value from [decoder] *)

val decode_json_field_descriptor_proto : Yojson.Basic.t -> field_descriptor_proto
(** [decode_json_field_descriptor_proto decoder] decodes a [field_descriptor_proto] value from [decoder] *)

val decode_json_enum_value_options : Yojson.Basic.t -> enum_value_options
(** [decode_json_enum_value_options decoder] decodes a [enum_value_options] value from [decoder] *)

val decode_json_enum_value_descriptor_proto : Yojson.Basic.t -> enum_value_descriptor_proto
(** [decode_json_enum_value_descriptor_proto decoder] decodes a [enum_value_descriptor_proto] value from [decoder] *)

val decode_json_enum_options : Yojson.Basic.t -> enum_options
(** [decode_json_enum_options decoder] decodes a [enum_options] value from [decoder] *)

val decode_json_enum_descriptor_proto_enum_reserved_range : Yojson.Basic.t -> enum_descriptor_proto_enum_reserved_range
(** [decode_json_enum_descriptor_proto_enum_reserved_range decoder] decodes a [enum_descriptor_proto_enum_reserved_range] value from [decoder] *)

val decode_json_enum_descriptor_proto : Yojson.Basic.t -> enum_descriptor_proto
(** [decode_json_enum_descriptor_proto decoder] decodes a [enum_descriptor_proto] value from [decoder] *)

val decode_json_extension_range_options : Yojson.Basic.t -> extension_range_options
(** [decode_json_extension_range_options decoder] decodes a [extension_range_options] value from [decoder] *)

val decode_json_descriptor_proto_extension_range : Yojson.Basic.t -> descriptor_proto_extension_range
(** [decode_json_descriptor_proto_extension_range decoder] decodes a [descriptor_proto_extension_range] value from [decoder] *)

val decode_json_oneof_options : Yojson.Basic.t -> oneof_options
(** [decode_json_oneof_options decoder] decodes a [oneof_options] value from [decoder] *)

val decode_json_oneof_descriptor_proto : Yojson.Basic.t -> oneof_descriptor_proto
(** [decode_json_oneof_descriptor_proto decoder] decodes a [oneof_descriptor_proto] value from [decoder] *)

val decode_json_message_options : Yojson.Basic.t -> message_options
(** [decode_json_message_options decoder] decodes a [message_options] value from [decoder] *)

val decode_json_descriptor_proto_reserved_range : Yojson.Basic.t -> descriptor_proto_reserved_range
(** [decode_json_descriptor_proto_reserved_range decoder] decodes a [descriptor_proto_reserved_range] value from [decoder] *)

val decode_json_descriptor_proto : Yojson.Basic.t -> descriptor_proto
(** [decode_json_descriptor_proto decoder] decodes a [descriptor_proto] value from [decoder] *)

val decode_json_method_options_idempotency_level : Yojson.Basic.t -> method_options_idempotency_level
(** [decode_json_method_options_idempotency_level decoder] decodes a [method_options_idempotency_level] value from [decoder] *)

val decode_json_method_options : Yojson.Basic.t -> method_options
(** [decode_json_method_options decoder] decodes a [method_options] value from [decoder] *)

val decode_json_method_descriptor_proto : Yojson.Basic.t -> method_descriptor_proto
(** [decode_json_method_descriptor_proto decoder] decodes a [method_descriptor_proto] value from [decoder] *)

val decode_json_service_options : Yojson.Basic.t -> service_options
(** [decode_json_service_options decoder] decodes a [service_options] value from [decoder] *)

val decode_json_service_descriptor_proto : Yojson.Basic.t -> service_descriptor_proto
(** [decode_json_service_descriptor_proto decoder] decodes a [service_descriptor_proto] value from [decoder] *)

val decode_json_file_options_optimize_mode : Yojson.Basic.t -> file_options_optimize_mode
(** [decode_json_file_options_optimize_mode decoder] decodes a [file_options_optimize_mode] value from [decoder] *)

val decode_json_file_options : Yojson.Basic.t -> file_options
(** [decode_json_file_options decoder] decodes a [file_options] value from [decoder] *)

val decode_json_source_code_info_location : Yojson.Basic.t -> source_code_info_location
(** [decode_json_source_code_info_location decoder] decodes a [source_code_info_location] value from [decoder] *)

val decode_json_source_code_info : Yojson.Basic.t -> source_code_info
(** [decode_json_source_code_info decoder] decodes a [source_code_info] value from [decoder] *)

val decode_json_file_descriptor_proto : Yojson.Basic.t -> file_descriptor_proto
(** [decode_json_file_descriptor_proto decoder] decodes a [file_descriptor_proto] value from [decoder] *)

val decode_json_file_descriptor_set : Yojson.Basic.t -> file_descriptor_set
(** [decode_json_file_descriptor_set decoder] decodes a [file_descriptor_set] value from [decoder] *)

val decode_json_generated_code_info_annotation : Yojson.Basic.t -> generated_code_info_annotation
(** [decode_json_generated_code_info_annotation decoder] decodes a [generated_code_info_annotation] value from [decoder] *)

val decode_json_generated_code_info : Yojson.Basic.t -> generated_code_info
(** [decode_json_generated_code_info decoder] decodes a [generated_code_info] value from [decoder] *)
