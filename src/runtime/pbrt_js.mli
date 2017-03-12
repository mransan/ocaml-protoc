
exception Unexpected_json_type of string * string 
  (* TODO expand on expected vs received type *)

module type Decoder_sig = sig 


  type t 
    (* Dictionary *) 

  type value = 
    | String of string 
    | Float of float 
    | Int of int 
    | Object of t 
    | Array_as_list of value list
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
