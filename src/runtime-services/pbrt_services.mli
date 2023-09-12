(** Runtime for Protobuf services. *)

module Errors = Errors
(** RPC errors. These are printable and serializable. *)

type rpc_error = Errors.rpc_error =
  | Invalid_binary of string
  | Invalid_json of string
  | Timeout of Errors.timeout_info
  | Server_error of string
  | Transport_error of string
  | Unknown_error

val pp_rpc_error : Format.formatter -> rpc_error -> unit

(** Whether there's a single value or a stream of them *)
module Value_mode : sig
  type unary
  type stream
end

module Pull_stream : sig
  type 'a t = { pull: 'ret. unit -> on_result:('a option -> 'ret) -> unit }
  (** Stream of incoming values, we can pull them out
      one by one until [None] is returned. *)
end

module Push_stream : sig
  type 'a t = {
    push: 'a -> unit;
    close: unit -> unit;
  }
  (** Stream of outgoing values, we can push new ones until we close it *)

  val push : 'a t -> 'a -> unit
  val close : _ t -> unit
end

(** Service stubs, client side *)
module Client : sig
  type _ mode =
    | Unary : Value_mode.unary mode
    | Stream : Value_mode.stream mode

  type ('req, 'req_mode, 'res, 'res_mode) rpc = {
    service_name: string;
    package: string list;  (** Package for the service *)
    rpc_name: string;
    req_mode: 'req_mode mode;
    res_mode: 'res_mode mode;
    encode_json_req: 'req -> Yojson.Basic.t;
    encode_pb_req: 'req -> Pbrt.Encoder.t -> unit;
    decode_json_res: Yojson.Basic.t -> 'res;
    decode_pb_res: Pbrt.Decoder.t -> 'res;
  }
  (** A RPC description. You need a transport library
      that knows where to send the bytes to actually use it. *)

  val mk_rpc :
    ?package:string list ->
    service_name:string ->
    rpc_name:string ->
    req_mode:'req_mode mode ->
    res_mode:'res_mode mode ->
    encode_json_req:('req -> Yojson.Basic.t) ->
    encode_pb_req:('req -> Pbrt.Encoder.t -> unit) ->
    decode_json_res:(Yojson.Basic.t -> 'res) ->
    decode_pb_res:(Pbrt.Decoder.t -> 'res) ->
    unit ->
    ('req, 'req_mode, 'res, 'res_mode) rpc
end

(** Service stubs, server side *)
module Server : sig
  type ('req, 'res) handler =
    | Unary : ('req -> 'res) -> ('req, 'res) handler
    | Client_stream : ('req Pull_stream.t -> 'res) -> ('req, 'res) handler
    | Server_stream : ('req -> 'res Push_stream.t) -> ('req, 'res) handler
    | Both_stream :
        ('req Pull_stream.t -> 'res Push_stream.t)
        -> ('req, 'res) handler

  (** A RPC endpoint. *)
  type rpc =
    | RPC : {
        name: string;
        f: ('req, 'res) handler;
        encode_json_res: 'res -> Yojson.Basic.t;
        encode_pb_res: 'res -> Pbrt.Encoder.t -> unit;
        decode_json_req: Yojson.Basic.t -> 'req;
        decode_pb_req: Pbrt.Decoder.t -> 'req;
      }
        -> rpc

  val mk_rpc :
    name:string ->
    f:('req, 'res) handler ->
    encode_json_res:('res -> Yojson.Basic.t) ->
    encode_pb_res:('res -> Pbrt.Encoder.t -> unit) ->
    decode_json_req:(Yojson.Basic.t -> 'req) ->
    decode_pb_req:(Pbrt.Decoder.t -> 'req) ->
    unit ->
    rpc
  (** Helper to build a RPC *)

  type t = {
    service_name: string;
    package: string list;
        (** The package this belongs in (e.g. "bigco.auth.secretpasswordstash"),
         split along "." *)
    handlers: rpc list;
  }
  (** A service with fixed set of methods. *)
end
