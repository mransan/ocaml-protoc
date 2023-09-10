
(** Code for twirp_error.proto *)

(* generated from "twirp_error.proto", do not edit *)



(** {2 Types} *)

type error = {
  code : string;
  msg : string;
}


(** {2 Basic values} *)

val default_error : 
  ?code:string ->
  ?msg:string ->
  unit ->
  error
(** [default_error ()] is the default value for type [error] *)


(** {2 Formatters} *)

val pp_error : Format.formatter -> error -> unit 
(** [pp_error v] formats v *)


(** {2 Protobuf YoJson Encoding} *)

val encode_json_error : error -> Yojson.Basic.t
(** [encode_json_error v encoder] encodes [v] to to json *)


(** {2 JSON Decoding} *)

val decode_json_error : Yojson.Basic.t -> error
(** [decode_json_error decoder] decodes a [error] value from [decoder] *)


(** {2 Services} *)
