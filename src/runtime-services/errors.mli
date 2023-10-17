
(** Code for errors.proto *)

(* generated from "errors.proto", do not edit *)



(** {2 Types} *)

type empty = unit

type timeout_info = {
  timeout_s : float;
}

type rpc_error =
  | Invalid_binary of string
  | Invalid_json of string
  | Timeout of timeout_info
  | Server_error of string
  | Transport_error of string
  | Unknown_error


(** {2 Basic values} *)

val default_empty : unit
(** [default_empty ()] is the default value for type [empty] *)

val default_timeout_info : 
  ?timeout_s:float ->
  unit ->
  timeout_info
(** [default_timeout_info ()] is the default value for type [timeout_info] *)

val default_rpc_error : unit -> rpc_error
(** [default_rpc_error ()] is the default value for type [rpc_error] *)


(** {2 Formatters} *)

val pp_empty : Format.formatter -> empty -> unit 
(** [pp_empty v] formats v *)

val pp_timeout_info : Format.formatter -> timeout_info -> unit 
(** [pp_timeout_info v] formats v *)

val pp_rpc_error : Format.formatter -> rpc_error -> unit 
(** [pp_rpc_error v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_empty : empty -> Pbrt.Encoder.t -> unit
(** [encode_pb_empty v encoder] encodes [v] with the given [encoder] *)

val encode_pb_timeout_info : timeout_info -> Pbrt.Encoder.t -> unit
(** [encode_pb_timeout_info v encoder] encodes [v] with the given [encoder] *)

val encode_pb_rpc_error : rpc_error -> Pbrt.Encoder.t -> unit
(** [encode_pb_rpc_error v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_empty : Pbrt.Decoder.t -> empty
(** [decode_pb_empty decoder] decodes a [empty] binary value from [decoder] *)

val decode_pb_timeout_info : Pbrt.Decoder.t -> timeout_info
(** [decode_pb_timeout_info decoder] decodes a [timeout_info] binary value from [decoder] *)

val decode_pb_rpc_error : Pbrt.Decoder.t -> rpc_error
(** [decode_pb_rpc_error decoder] decodes a [rpc_error] binary value from [decoder] *)


(** {2 Protobuf YoJson Encoding} *)

val encode_json_empty : empty -> Yojson.Basic.t
(** [encode_json_empty v encoder] encodes [v] to to json *)

val encode_json_timeout_info : timeout_info -> Yojson.Basic.t
(** [encode_json_timeout_info v encoder] encodes [v] to to json *)

val encode_json_rpc_error : rpc_error -> Yojson.Basic.t
(** [encode_json_rpc_error v encoder] encodes [v] to to json *)


(** {2 JSON Decoding} *)

val decode_json_empty : Yojson.Basic.t -> empty
(** [decode_json_empty decoder] decodes a [empty] value from [decoder] *)

val decode_json_timeout_info : Yojson.Basic.t -> timeout_info
(** [decode_json_timeout_info decoder] decodes a [timeout_info] value from [decoder] *)

val decode_json_rpc_error : Yojson.Basic.t -> rpc_error
(** [decode_json_rpc_error decoder] decodes a [rpc_error] value from [decoder] *)


(** {2 Services} *)
