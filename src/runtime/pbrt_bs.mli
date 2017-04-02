(** Protobuf BuckleScript Runtime *)

(** {2 Decoding exceptions} *) 

module E : sig 

  type error 

  exception Failure of error 

  val unexpected_json_type : string -> string -> 'a 
  (** [unexpected_json_type record_name field_name] @raises Failure *)

  val malformed_variant : string -> 'a 
  (** [malformed_variant variant_name] @raises Failure *)

end 

(** {2 Decoding functions} *)

val int32 : Js_json.t -> string -> string -> int32 
(** [int32 json record_name field_name] converts [json] to the int32 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid int32 
    encoding *)

val int64 : Js_json.t -> string -> string -> int64 
(** [int64 json record_name field_name] converts [json] to the int64 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid int64 
    encoding *)

val int : Js_json.t -> string -> string -> int 
(** [int json record_name field_name] converts [json] to the int 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid int 
    encoding *)

val float : Js_json.t -> string -> string -> float 
(** [float json record_name field_name] converts [json] to the float 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid float 
    encoding *)

val bool : Js_json.t -> string -> string -> bool 
(** [bool json record_name field_name] converts [json] to the bool 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid bool 
    encoding *)

val string : Js_json.t -> string -> string -> string 
(** [string json record_name field_name] converts [json] to the string 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid string 
    encoding *)

val bytes : Js_json.t -> string -> string -> bytes 
(** [bytes json record_name field_name] converts [json] to the bytes 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid bytes 
    encoding *)

val object_ : Js_json.t -> string -> string -> Js_json.t Js_dict.t
(** [object_ json record_name field_name] converts [json] to a JS object
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid object
    encoding *)

val array_ : Js_json.t -> string -> string -> Js_json.t array 
(** [array_ json record_name field_name] converts [json] to a JS array 
    value according to the Protobuf specifications. 
    
    @raise Failure if the [json] value does not contain a valid array 
    encoding *)
