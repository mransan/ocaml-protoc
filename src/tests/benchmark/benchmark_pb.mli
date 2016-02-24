type test_type =
  | Encode of int
  | Decode

val decode_test_type : Pbrt.Decoder.t -> test_type
(** [decode_test_type decoder] decodes a [test_type] value from [decoder] *)

val encode_test_type : test_type -> Pbrt.Encoder.t -> unit
(** [encode_test_type v encoder] encodes [v] with the given [encoder] *)

val pp_test_type : Format.formatter -> test_type -> unit 
(** [pp_test_type v] formats v] *)

val default_test_type : unit -> test_type
(** [default_test_type ()] is the default value for type [test_type] *)

type test_id =
  | Int_list 
  | Int_repeated 
  | Int_packed_repeated 

val decode_test_id : Pbrt.Decoder.t -> test_id
(** [decode_test_id decoder] decodes a [test_id] value from [decoder] *)

val encode_test_id : test_id -> Pbrt.Encoder.t -> unit
(** [encode_test_id v encoder] encodes [v] with the given [encoder] *)

val pp_test_id : Format.formatter -> test_id -> unit 
(** [pp_test_id v] formats v] *)

val default_test_id : unit -> test_id
(** [default_test_id ()] is the default value for type [test_id] *)

type test_request = {
  mutable type_ : test_type;
  mutable file_name : string;
  mutable test_id : test_id;
}

val decode_test_request : Pbrt.Decoder.t -> test_request
(** [decode_test_request decoder] decodes a [test_request] value from [decoder] *)

val encode_test_request : test_request -> Pbrt.Encoder.t -> unit
(** [encode_test_request v encoder] encodes [v] with the given [encoder] *)

val pp_test_request : Format.formatter -> test_request -> unit 
(** [pp_test_request v] formats v] *)

val default_test_request : unit -> test_request
(** [default_test_request ()] is the default value for type [test_request] *)

type test_requests = {
  mutable requests : test_requests list;
}

val decode_test_requests : Pbrt.Decoder.t -> test_requests
(** [decode_test_requests decoder] decodes a [test_requests] value from [decoder] *)

val encode_test_requests : test_requests -> Pbrt.Encoder.t -> unit
(** [encode_test_requests v encoder] encodes [v] with the given [encoder] *)

val pp_test_requests : Format.formatter -> test_requests -> unit 
(** [pp_test_requests v] formats v] *)

val default_test_requests : unit -> test_requests
(** [default_test_requests ()] is the default value for type [test_requests] *)

type encode_data = {
  mutable creation_time : float;
  mutable encode_time : float;
  mutable to_file_time : float;
}

val decode_encode_data : Pbrt.Decoder.t -> encode_data
(** [decode_encode_data decoder] decodes a [encode_data] value from [decoder] *)

val encode_encode_data : encode_data -> Pbrt.Encoder.t -> unit
(** [encode_encode_data v encoder] encodes [v] with the given [encoder] *)

val pp_encode_data : Format.formatter -> encode_data -> unit 
(** [pp_encode_data v] formats v] *)

val default_encode_data : unit -> encode_data
(** [default_encode_data ()] is the default value for type [encode_data] *)

type decode_data = {
  mutable from_file_time : float;
  mutable decode_time : float;
}

val decode_decode_data : Pbrt.Decoder.t -> decode_data
(** [decode_decode_data decoder] decodes a [decode_data] value from [decoder] *)

val encode_decode_data : decode_data -> Pbrt.Encoder.t -> unit
(** [encode_decode_data v encoder] encodes [v] with the given [encoder] *)

val pp_decode_data : Format.formatter -> decode_data -> unit 
(** [pp_decode_data v] formats v] *)

val default_decode_data : unit -> decode_data
(** [default_decode_data ()] is the default value for type [decode_data] *)

type test_response_data =
  | Encode of encode_data
  | Decode of decode_data

and test_response = {
  mutable difficulty_size : int;
  mutable test_id : test_id;
  mutable data : test_response_data;
}

val decode_test_response : Pbrt.Decoder.t -> test_response
(** [decode_test_response decoder] decodes a [test_response] value from [decoder] *)

val encode_test_response : test_response -> Pbrt.Encoder.t -> unit
(** [encode_test_response v encoder] encodes [v] with the given [encoder] *)

val pp_test_response_data : Format.formatter -> test_response_data -> unit 
(** [pp_test_response_data v] formats v] *)

val pp_test_response : Format.formatter -> test_response -> unit 
(** [pp_test_response v] formats v] *)

val default_test_response : unit -> test_response
(** [default_test_response ()] is the default value for type [test_response] *)

type test_responses = {
  mutable responses : test_responses list;
}

val decode_test_responses : Pbrt.Decoder.t -> test_responses
(** [decode_test_responses decoder] decodes a [test_responses] value from [decoder] *)

val encode_test_responses : test_responses -> Pbrt.Encoder.t -> unit
(** [encode_test_responses v encoder] encodes [v] with the given [encoder] *)

val pp_test_responses : Format.formatter -> test_responses -> unit 
(** [pp_test_responses v] formats v] *)

val default_test_responses : unit -> test_responses
(** [default_test_responses ()] is the default value for type [test_responses] *)

type int_list = {
  mutable int_list : int list;
}

val decode_int_list : Pbrt.Decoder.t -> int_list
(** [decode_int_list decoder] decodes a [int_list] value from [decoder] *)

val encode_int_list : int_list -> Pbrt.Encoder.t -> unit
(** [encode_int_list v encoder] encodes [v] with the given [encoder] *)

val pp_int_list : Format.formatter -> int_list -> unit 
(** [pp_int_list v] formats v] *)

val default_int_list : unit -> int_list
(** [default_int_list ()] is the default value for type [int_list] *)

type int_repeated = {
  int_repeated : int Pbrt.Repeated_field.t;
}

val decode_int_repeated : Pbrt.Decoder.t -> int_repeated
(** [decode_int_repeated decoder] decodes a [int_repeated] value from [decoder] *)

val encode_int_repeated : int_repeated -> Pbrt.Encoder.t -> unit
(** [encode_int_repeated v encoder] encodes [v] with the given [encoder] *)

val pp_int_repeated : Format.formatter -> int_repeated -> unit 
(** [pp_int_repeated v] formats v] *)

val default_int_repeated : unit -> int_repeated
(** [default_int_repeated ()] is the default value for type [int_repeated] *)

type int_packed_repeated = {
  int_packed_repeated : int Pbrt.Repeated_field.t;
}

val decode_int_packed_repeated : Pbrt.Decoder.t -> int_packed_repeated
(** [decode_int_packed_repeated decoder] decodes a [int_packed_repeated] value from [decoder] *)

val encode_int_packed_repeated : int_packed_repeated -> Pbrt.Encoder.t -> unit
(** [encode_int_packed_repeated v encoder] encodes [v] with the given [encoder] *)

val pp_int_packed_repeated : Format.formatter -> int_packed_repeated -> unit 
(** [pp_int_packed_repeated v] formats v] *)

val default_int_packed_repeated : unit -> int_packed_repeated
(** [default_int_packed_repeated ()] is the default value for type [int_packed_repeated] *)

