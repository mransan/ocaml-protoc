(** unittest.proto Generated Types and Encoding *)


(** {2 Types} *)

type all_basic_types = {
  field01 : float;
  field02 : float;
  field03 : int32;
  field04 : int64;
  field05 : int32;
  field06 : int64;
  field07 : int32;
  field08 : int64;
  field09 : int32;
  field10 : int64;
  field13 : bool;
  field14 : string;
  repeated01 : float list;
  repeated02 : float list;
  repeated03 : int32 list;
  repeated04 : int64 list;
  repeated05 : int32 list;
  repeated06 : int64 list;
  repeated07 : int32 list;
  repeated08 : int64 list;
  repeated09 : int32 list;
  repeated10 : int64 list;
  repeated13 : bool list;
  repeated14 : string list;
}

type small_message = {
  sm_string : string;
}

type enum =
  | Value0 
  | Value1 
  | Value_two 

type single_one_of =
  | String_value of string
  | Int_value of int32
  | Enum_value of enum
  | Small_message of small_message
  | Recursive_value of single_one_of

type test = {
  all_basic_types : all_basic_types option;
  test_enum0 : enum;
  test_enum1 : enum;
  test_enum2 : enum;
  single_one_of_string : single_one_of option;
  single_one_of_int : single_one_of option;
  single_one_of_enum : single_one_of option;
  single_one_of_small_message : single_one_of option;
  single_one_of_recursive : single_one_of option;
}


(** {2 Default values} *)

val default_all_basic_types : 
  ?field01:float ->
  ?field02:float ->
  ?field03:int32 ->
  ?field04:int64 ->
  ?field05:int32 ->
  ?field06:int64 ->
  ?field07:int32 ->
  ?field08:int64 ->
  ?field09:int32 ->
  ?field10:int64 ->
  ?field13:bool ->
  ?field14:string ->
  ?repeated01:float list ->
  ?repeated02:float list ->
  ?repeated03:int32 list ->
  ?repeated04:int64 list ->
  ?repeated05:int32 list ->
  ?repeated06:int64 list ->
  ?repeated07:int32 list ->
  ?repeated08:int64 list ->
  ?repeated09:int32 list ->
  ?repeated10:int64 list ->
  ?repeated13:bool list ->
  ?repeated14:string list ->
  unit ->
  all_basic_types
(** [default_all_basic_types ()] is the default value for type [all_basic_types] *)

val default_small_message : 
  ?sm_string:string ->
  unit ->
  small_message
(** [default_small_message ()] is the default value for type [small_message] *)

val default_enum : unit -> enum
(** [default_enum ()] is the default value for type [enum] *)

val default_single_one_of : unit -> single_one_of
(** [default_single_one_of ()] is the default value for type [single_one_of] *)

val default_test : 
  ?all_basic_types:all_basic_types option ->
  ?test_enum0:enum ->
  ?test_enum1:enum ->
  ?test_enum2:enum ->
  ?single_one_of_string:single_one_of option ->
  ?single_one_of_int:single_one_of option ->
  ?single_one_of_enum:single_one_of option ->
  ?single_one_of_small_message:single_one_of option ->
  ?single_one_of_recursive:single_one_of option ->
  unit ->
  test
(** [default_test ()] is the default value for type [test] *)

  
  (** {2 BS Decoding} *)
  
  val decode_all_basic_types : Js_json.t Js_dict.t -> all_basic_types
  (** [decode_all_basic_types decoder] decodes a [all_basic_types] value from [decoder] *)
  
  val decode_small_message : Js_json.t Js_dict.t -> small_message
  (** [decode_small_message decoder] decodes a [small_message] value from [decoder] *)
  
  val decode_enum : Js_json.t -> enum
  (** [decode_enum value] decodes a [enum] from a Json value*)
  
  val decode_single_one_of : Js_json.t Js_dict.t -> single_one_of
  (** [decode_single_one_of decoder] decodes a [single_one_of] value from [decoder] *)
  
  val decode_test : Js_json.t Js_dict.t -> test
  (** [decode_test decoder] decodes a [test] value from [decoder] *)
  
  
  (** {2 Protobuf JSON Encoding} *)
  
  val encode_all_basic_types : all_basic_types -> Js_json.t Js_dict.t -> unit
  (** [encode_all_basic_types v encoder] encodes [v] with the given [encoder] *)
  
  val encode_small_message : small_message -> Js_json.t Js_dict.t -> unit
  (** [encode_small_message v encoder] encodes [v] with the given [encoder] *)
  
  val encode_enum : enum -> string
  (** [encode_enum v] returns JSON string*)
  
  val encode_single_one_of : single_one_of -> Js_json.t Js_dict.t -> unit
  (** [encode_single_one_of v encoder] encodes [v] with the given [encoder] *)
  
  val encode_test : test -> Js_json.t Js_dict.t -> unit
  (** [encode_test v encoder] encodes [v] with the given [encoder] *)
  