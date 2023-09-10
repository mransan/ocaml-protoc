(** Service stubs, client side *)
module Client : sig
  type error =
    | Transport_error of string
    | Timeout
    | Decode_error_json of string
    | Decode_error_binary of Pbrt.Decoder.error

  type transport = {
    query:
      'ret.
      service_name:string ->
      rpc_name:string ->
      [ `JSON | `BINARY ] ->
      string ->
      on_result:((string, error) result -> 'ret) ->
      'ret;
  }
  (** A transport method, ie. a way to query a remote service
     by sending it a query, and register a callback to be
     called when the response is received.

     The [query] function is called like so:
     [transport.query ~service_name ~rpc_name encoding req ~on_result],
     where [rpc_name] is the name of the method in the service [service_name],
    [req] is the encoded query using [encoding], and [on_result] is a callback
    that will be called after the service comes back with a response.
  *)

  type ('req, 'ret) rpc = {
    call:
      'actual_ret.
      [ `JSON | `BINARY ] ->
      transport ->
      'req ->
      on_result:(('ret, error) result -> 'actual_ret) ->
      'actual_ret;
  }
  (** A RPC. By calling it with a concrete transport, one gets a future result. *)

  val mk_rpc :
    service_name:string ->
    rpc_name:string ->
    encode_json_req:('req -> Yojson.Safe.t) ->
    encode_pb_req:('req -> Pbrt.Encoder.t -> unit) ->
    decode_json_res:(Yojson.Safe.t -> 'res) ->
    decode_pb_res:(Pbrt.Decoder.t -> 'res) ->
    unit ->
    ('req, 'res) rpc
end

(** Service stubs, server side *)
module Server : sig
  (** Errors that can arise during request processing. *)
  type error =
    | Invalid_json
    | Invalid_pb of Pbrt.Decoder.error
    | Handler_failed of string

  type rpc = {
    rpc_name: string;
    rpc_handler: [ `JSON | `BINARY ] -> string -> (string, error) result;
  }

  val mk_rpc :
    name:string ->
    f:('req -> 'res) ->
    encode_json_res:('res -> string) ->
    encode_pb_res:('res -> Pbrt.Encoder.t -> unit) ->
    decode_json_req:(Yojson.Safe.t -> 'req) ->
    decode_pb_req:(Pbrt.Decoder.t -> 'req) ->
    unit ->
    rpc
  (** Helper to build a RPC *)

  (** A RPC implementation. *)

  type t = {
    name: string;
    handlers: rpc list;
  }
  (** A service with fixed set of methods. *)
end
