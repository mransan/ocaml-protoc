exception Unexpected_json_type of string * string 
  (* TODO expand on expected vs received type *)

let unexpected_json_type record_name field_name = 
  raise (Unexpected_json_type (record_name, field_name))

module type Decoder_sig = sig 

  type t 
    (* Dictionary *) 

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
    | _ -> unexpected_json_type record_name field_name  
  
  let float v record_name field_name = 
    match v with
    | D.String v -> float_of_string v 
    | D.Float f -> f 
    | D.Int i -> float_of_int i 
    | D.Null -> 0.0 
    | _ -> unexpected_json_type record_name field_name  

  let int64 v record_name field_name = 
    match v with
    | D.String v -> Int64.of_string v 
    | D.Float f -> Int64.of_float f 
    | D.Int i -> Int64.of_int i 
    | D.Null -> 0L 
    | _ -> unexpected_json_type record_name field_name  

  let int v record_name field_name = 
    match v with
    | D.String v -> int_of_string v 
    | D.Float f -> int_of_float f 
    | D.Int i -> i 
    | D.Null -> 0 
    | _ -> unexpected_json_type record_name field_name  

  let string v record_name field_name = 
    match v with
    | D.String v -> v 
    | D.Null -> ""
    | _ -> unexpected_json_type record_name field_name

  let bool v record_name field_name = 
    match v with
    | D.Bool b -> b 
    | D.Null -> false
    | _ -> unexpected_json_type record_name field_name 

  let bytes _ record_name field_name = 
    unexpected_json_type record_name field_name
end 
