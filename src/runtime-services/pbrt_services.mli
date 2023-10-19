(** Runtime for Protobuf services. *)

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
  type ('req, 'res, 'state) client_stream_handler_with_state = {
    init: unit -> 'state;  (** When a stream starts *)
    on_item: 'state -> 'req -> unit;
        (** When an element of the stream is received. This can either
         update the internal state by mutation, performing side effects,
         or choose to return a value early and stop reading from the input stream. *)
    on_close: 'state -> 'res;  (** When the stream is over *)
  }
  (** Handler that receives a client stream *)

  type ('req, 'res) client_stream_handler =
    | Client_stream_handler :
        ('req, 'res, 'state) client_stream_handler_with_state
        -> ('req, 'res) client_stream_handler
  [@@unboxed]

  type ('req, 'res) server_stream_handler = 'req -> 'res Push_stream.t -> unit
  (** Takes the input value and a push stream (to send items to
      the caller, and then close the stream at the end).
      The stream's [close] function must be called exactly once. *)

  type ('req, 'res, 'state) bidirectional_stream_handler_with_state = {
    init: unit -> 'res Push_stream.t -> 'state;
    on_item: 'state -> 'req -> unit;
    on_close: 'state -> unit;
  }
  (** Handler taking a stream of values and returning a stream as well. *)

  type ('req, 'res) bidirectional_stream_handler =
    | Bidirectional_stream_handler :
        ('req, 'res, 'state) bidirectional_stream_handler_with_state
        -> ('req, 'res) bidirectional_stream_handler
  [@@unboxed]

  (** A handler, i.e the server side implementation of a single RPC method.
      Handlers come in various flavors because they make take, or return,
      streams of values. *)
  type ('req, 'res) handler =
    | Unary of ('req -> 'res)
        (** Simple unary handler, gets a value, returns a value. *)
    | Client_stream of ('req, 'res) client_stream_handler
    | Server_stream of ('req, 'res) server_stream_handler
    | Bidirectional_stream of ('req, 'res) bidirectional_stream_handler

  type ('req, 'res) rpc = {
    name: string;
    f: ('req, 'res) handler;
    encode_json_res: 'res -> Yojson.Basic.t;
    encode_pb_res: 'res -> Pbrt.Encoder.t -> unit;
    decode_json_req: Yojson.Basic.t -> 'req;
    decode_pb_req: Pbrt.Decoder.t -> 'req;
  }

  (** A RPC endpoint. *)
  type any_rpc = RPC : ('req, 'res) rpc -> any_rpc [@@unboxed]

  val mk_rpc :
    name:string ->
    f:('req, 'res) handler ->
    encode_json_res:('res -> Yojson.Basic.t) ->
    encode_pb_res:('res -> Pbrt.Encoder.t -> unit) ->
    decode_json_req:(Yojson.Basic.t -> 'req) ->
    decode_pb_req:(Pbrt.Decoder.t -> 'req) ->
    unit ->
    any_rpc
  (** Helper to build a RPC *)

  type t = {
    service_name: string;
    package: string list;
        (** The package this belongs in (e.g. "bigco.auth.secretpasswordstash"),
         split along "." *)
    handlers: any_rpc list;
  }
  (** A service with fixed set of methods. *)
end
