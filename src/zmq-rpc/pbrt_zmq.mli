(** RPC based on ZMQ as a transport. *)

open Pbrt_services

module Meta = Meta
(** Metadata for the RPC itself *)

type 'a or_error = ('a, Meta.error) result

val pp_error : Format.formatter -> Meta.error -> unit

module Server : sig
  type t

  val create :
    ?spawn:((unit -> unit) -> unit) ->
    ?active:bool Atomic.t ->
    Zmq.Context.t ->
    addr:string ->
    Pbrt_services.Server.t list ->
    t

  val stop : t -> unit
  val run : t -> unit
end

module Client : sig
  type t

  val create : Zmq.Context.t -> addr:string -> t
  val dispose : t -> unit

  val call :
    t ->
    ?timeout:float ->
    ('req, Value_mode.unary, 'res, Value_mode.unary) Client.rpc ->
    'req ->
    on_result:('res or_error -> unit) ->
    unit

  val call_block :
    t ->
    ?timeout:float ->
    ('req, Value_mode.unary, 'res, Value_mode.unary) Client.rpc ->
    'req ->
    'res or_error

  val call_ret_stream :
    t ->
    ('req, Value_mode.unary, 'res, Value_mode.stream) Client.rpc ->
    'req ->
    on_event:([ `Done | `Error of Meta.error | `Yield of 'res ] -> unit) ->
    unit
end
