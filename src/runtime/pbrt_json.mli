(** Protobuf JSON encoding runtime *)

(** All exception which could be raised by the generated JSON encoder
    and decode function *)
module E : sig 
  type error = 
    | Unexpected_json_type of string * string  
    | Malformed_variant of string  

  exception Failure of error
  (** Decoding/Encoding failure *)

  val unexpected_json_type : string -> string -> 'a
  (** [unexpected_json_type record_name field_name] raises 
      [Failure (Unexpected_json_type (record_name, field_name))] *)

  val malformed_variant : string -> 'a
  (** [malformed_variant variant_name] raise 
      [Failure (Malformed_variant variant_name)] *)
end 

(** Module signature which the generated code relies upon for 
    decoding functions *)
module type Decoder_sig = sig 
  
  type t 
  (** Decoder, represents a JSON object *)

  type value = 
    | String of string 
    | Float of float 
    | Int of int 
    | Object of t 
    | Array_as_array of value array 
    | Bool of bool 
    | Null

  val key : t -> (string * value) option
  (** [key decoder] returns the next key/value pair in the current JSON 
      object. [None] indicates no more keys are available. *)

end 

(** Module signature which the gnerated code relies upon for 
    encoding functions *)
module type Encoder_sig = sig

  type t 
  (** Encoder, represents a JSON object *)

  val empty : unit -> t  

  val set_null : t -> string -> unit
  val set_string : t -> string -> string -> unit 
  val set_float : t -> string -> float -> unit 
  val set_int : t -> string -> int -> unit  
  val set_bool : t -> string -> bool -> unit 
  val set_object : t -> string -> t -> unit 
  
  val set_string_list : t -> string -> string list -> unit 
  val set_float_list : t -> string -> float list -> unit 
  val set_int_list : t -> string -> int list -> unit  
  val set_bool_list : t -> string -> bool list -> unit 
  val set_object_list : t -> string -> t list -> unit

end

(** Helper module for the generated code for common 
    functionality *)
module Make_decoder_helper(D:Decoder_sig) : sig 
  
  val string : D.value -> string -> string -> string 
  val float : D.value -> string -> string -> float 
  val int32 : D.value -> string -> string -> int32 
  val int64 : D.value -> string -> string -> int64 
  val int : D.value -> string -> string -> int 
  val bool : D.value -> string -> string -> bool
  val bytes : D.value -> string -> string -> bytes 

end 
