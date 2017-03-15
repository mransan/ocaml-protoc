module E = struct

  type error = 
    | Unexpected_json_type of string * string  
    | Malformed_variant of string  

  exception Failure of error 

  let unexpected_json_type record_name field_name = 
    raise (Failure (Unexpected_json_type (record_name, field_name)))

  let malformed_variant variant_name = 
    raise (Failure (Malformed_variant variant_name)) 

  let string_of_error = function
    | Unexpected_json_type (record_name, field_name) -> 
      Printf.sprintf "Unexpected json type (record name:%s, field_name:%s)"
        record_name field_name 
    | Malformed_variant variant_name ->
      Printf.sprintf "Malformed variant (variant name: %s)" variant_name

  let () =
    Printexc.register_printer (fun exn ->
      match exn with
      | Failure e -> Some (string_of_error e)
      | _ -> None
      )
  
end 

module type Decoder_sig = sig 

  type t 

  type value = 
    | String of string 
    | Float of float 
    | Int of int 
    | Object of t 
    | Array_as_array of value array 
    | Bool of bool 
    | Null

  val key : t -> (string * value) option

end 

module type Encoder_sig = sig

  type t 

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

module Make_decoder_helper(D:Decoder_sig) = struct 

  let int32 v record_name field_name = 
    match v with
    | D.String v -> Int32.of_string v 
    | D.Float f -> Int32.of_float f 
    | D.Int i -> Int32.of_int i 
    | D.Null -> 0l 
    | _ -> E.unexpected_json_type record_name field_name  
  
  let float v record_name field_name = 
    match v with
    | D.String v -> float_of_string v 
    | D.Float f -> f 
    | D.Int i -> float_of_int i 
    | D.Null -> 0.0 
    | _ -> E.unexpected_json_type record_name field_name  

  let int64 v record_name field_name = 
    match v with
    | D.String v -> Int64.of_string v 
    | D.Float f -> Int64.of_float f 
    | D.Int i -> Int64.of_int i 
    | D.Null -> 0L 
    | _ -> E.unexpected_json_type record_name field_name  

  let int v record_name field_name = 
    match v with
    | D.String v -> int_of_string v 
    | D.Float f -> int_of_float f 
    | D.Int i -> i 
    | D.Null -> 0 
    | _ -> E.unexpected_json_type record_name field_name  

  let string v record_name field_name = 
    match v with
    | D.String v -> v 
    | D.Null -> ""
    | _ -> E.unexpected_json_type record_name field_name

  let bool v record_name field_name = 
    match v with
    | D.Bool b -> b 
    | D.Null -> false
    | _ -> E.unexpected_json_type record_name field_name 

  let bytes _ record_name field_name = 
    E.unexpected_json_type record_name field_name
end 
