val caml_file_name_of_proto_file_name : string -> string 
(** [caml_file_name_of_proto_file_name f] returns the 
      ocaml base file name corresponding to the proto file name [f]. The
      extension (.ml or .mli) is the responsability of the caller. 
      
      It's important to note that similarly to Google protoc compiler, 
      the generated file name is a hard coding mapping to the proto filename. This
      is needed in order to independently generate included proto files. 
 *)

val gen_type : ?and_:unit -> Ocaml_types.type_ -> string 
(** [gen_type_const_variant v] generates the OCaml type declaration for [v]
 *)

val gen_decode :  ?and_:unit -> Ocaml_types.type_ -> string option 
(** [gen_decode_record record]  generates the function implementation for
    decoding a message into the given record type. 
 *)

val gen_decode_sig : Ocaml_types.type_ -> string option 
(** [gen_decode_sig_record record] generates the function signature for
    decoding a message into the given record type.
  *) 

val gen_encode : ?and_:unit -> Ocaml_types.type_ -> string option
(** [gen_encode t] generates the function signature for 
    encoding the given type [t] type into a protobuffer. 
  *)

val gen_encode_sig : Ocaml_types.type_ -> string option 
(** [gen_encode_sig record] generates the function signature for 
    encoding the given [record] type into a protobuffer. 
  *)

val gen_string_of : ?and_:unit -> Ocaml_types.type_ -> string option
(** [gen_string_of record] generates the function implementation for 
    computing a debug string of the given record
  *)

val gen_string_of_sig : Ocaml_types.type_ -> string option 
(** [gen_string_of_sig record] generates the function signature for 
    computing a debug string of the given record
 *)

(* --- Testing purpose onlye --- *)
val gen_mappings_record : Ocaml_types.record -> string
