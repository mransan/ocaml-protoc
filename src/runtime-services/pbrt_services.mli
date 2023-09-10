(** Runtime for Protobuf services. *)

module Errors = Errors
(** RPC errors. These are printable and serializable. *)

(** Service stubs, client side *)
module Client : sig
  type error = Errors.rpc_error =
    | Invalid_binary of string
    | Invalid_json of string
    | Timeout of Errors.timeout_info
    | Server_error of string
    | Transport_error of string
    | Unknown_error

  val pp_error : Format.formatter -> error -> unit

  type ('req, 'ret) rpc = {
    service_name: string;
    rpc_name: string;
    encode_json_req: 'req -> Yojson.Basic.t;
    encode_pb_req: 'req -> Pbrt.Encoder.t -> unit;
    decode_json_res: Yojson.Basic.t -> 'ret;
    decode_pb_res: Pbrt.Decoder.t -> 'ret;
  }
  (** A RPC description. You need a transport library
   that knows where to send the bytes to actually use it. *)

  val mk_rpc :
    service_name:string ->
    rpc_name:string ->
    encode_json_req:('req -> Yojson.Basic.t) ->
    encode_pb_req:('req -> Pbrt.Encoder.t -> unit) ->
    decode_json_res:(Yojson.Basic.t -> 'res) ->
    decode_pb_res:(Pbrt.Decoder.t -> 'res) ->
    unit ->
    ('req, 'res) rpc
end

(** Service stubs, server side *)
module Server : sig
  (** Errors that can arise during request processing. *)
  type error = Errors.rpc_error =
    | Invalid_binary of string
    | Invalid_json of string
    | Timeout of Errors.timeout_info
    | Server_error of string
    | Transport_error of string
    | Unknown_error

  val pp_error : Format.formatter -> error -> unit

  (** A RPC endpoint. *)
  type rpc =
    | RPC : {
        name: string;
        f: 'req -> 'res;
        encode_json_res: 'res -> Yojson.Basic.t;
        encode_pb_res: 'res -> Pbrt.Encoder.t -> unit;
        decode_json_req: Yojson.Basic.t -> 'req;
        decode_pb_req: Pbrt.Decoder.t -> 'req;
      }
        -> rpc

  val mk_rpc :
    name:string ->
    f:('req -> 'res) ->
    encode_json_res:('res -> Yojson.Basic.t) ->
    encode_pb_res:('res -> Pbrt.Encoder.t -> unit) ->
    decode_json_req:(Yojson.Basic.t -> 'req) ->
    decode_pb_req:(Pbrt.Decoder.t -> 'req) ->
    unit ->
    rpc
  (** Helper to build a RPC *)

  type t = {
    service_name: string;
    handlers: rpc list;
  }
  (** A service with fixed set of methods. *)
end
