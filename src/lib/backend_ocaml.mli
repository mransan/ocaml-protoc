(** Backend for compiling Protobuffer messages to OCaml 
 *)

(** This module focuses on the compilation steps which transforms a 
    fully resolved Protobuffer message into an OCaml representation. 

    After compilation this module also expose code generation 
    functionality. 
 *)

(** {2 Types} *)

type field_type = 
  | String 
  | Float 
  | Int 
  | Bytes
  | Bool
  | User_defined_type of string 

type field_name = string 

type type_qualifier = 
  | No_qualifier
  | Option
  | List 

(** the field is parametrized by the encoding_type with could either 
    [field_encoding] or [record_encoding_type] depending 
    if the field is used in a variant or record type
 *) 
type 'a ifield = {
  field_type : field_type; 
  field_name : field_name; 
  type_qualifier : type_qualifier; 
  encoding_type : 'a;
}

type 'a ivariant= {
  variant_name : string; 
  constructors : 'a list;
}

type const_variant_constructor = string * int  

type const_variant = const_variant_constructor ivariant 

type variant_constructor = Encoding_util.field_encoding ifield 

type variant = variant_constructor ivariant 

type record_encoding_type = 
  | Regular_field of Encoding_util.field_encoding
  | One_of        of variant  

type record = {
  record_name: string; 
  fields : record_encoding_type ifield list; 
}

type type_ = 
  | Record of record 
  | Variant of variant
  | Const_variant  of const_variant 

(** {2 Compilation } *) 

val compile :
  Pbtt.resolved Pbtt.proto ->
  Pbtt.resolved Pbtt.proto_type -> 
  type_ list 

(** {2 Code Generation} *)

module Codegen : sig
  
  val caml_file_name_of_proto_file_name : string -> string 
  (** [caml_file_name_of_proto_file_name f] returns the 
      ocaml base file name corresponding to the proto file name [f]. The
      extension (.ml or .mli) is the responsability of the caller. 
      
      It's important to note that similarly to Google protoc compiler, 
      the generated file name is a hard coding mapping to the proto filename. This
      is needed in order to independently generate included proto files. 
    *)

  val gen_type : ?and_:unit -> type_ -> string 
  (** [gen_type_const_variant v] generates the OCaml type declaration for [v]
   *)

  val gen_decode :  ?and_:unit -> type_ -> string option 
  (** [gen_decode_record record]  generates the function implementation for
      decoding a message into the given record type. 
   *)
  
  val gen_decode_sig : type_ -> string option 
  (** [gen_decode_sig_record record] generates the function signature for
      decoding a message into the given record type.
    *) 
  
  val gen_encode : ?and_:unit -> type_ -> string option
  (** [gen_encode t] generates the function signature for 
      encoding the given type [t] type into a protobuffer. 
    *)

  val gen_encode_sig : type_ -> string option 
  (** [gen_encode_sig record] generates the function signature for 
      encoding the given [record] type into a protobuffer. 
    *)

  val gen_string_of : ?and_:unit -> type_ -> string option
  (** [gen_string_of record] generates the function implementation for 
      computing a debug string of the given record
    *)

  val gen_string_of_sig : type_ -> string option 
  (** [gen_string_of_sig record] generates the function signature for 
      computing a debug string of the given record
   *)

  (* --- Testing purpose onlye --- *)
  val gen_mappings_record : record -> string

end

(* --- Testing purpose only --- *)

val type_name : string list -> string -> string 

val constructor_name : string -> string 

val type_name_of_message : Pbtt.type_scope -> Pbtt.type_scope -> string -> string

