(** Twirp for tiny_httpd.

    This implements the server end of https://github.com/twitchtv/twirp *)

module Twirp_error = Twirp_error
module Error_codes = Error_codes

val fail : ?msg:string -> Error_codes.t -> 'a
val failf : Error_codes.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** A request handler, specialized for Twirp. *)
type handler =
  | Handler : {
      rpc:
        ( 'req,
          Pbrt_services.Value_mode.unary,
          'res,
          Pbrt_services.Value_mode.unary )
        Pbrt_services.Server.rpc;
      f: 'req -> 'res;
    }
      -> handler

val mk_handler :
  ( 'req,
    Pbrt_services.Value_mode.unary,
    'res,
    Pbrt_services.Value_mode.unary )
  Pbrt_services.Server.rpc ->
  ('req -> 'res) ->
  handler
(** Create a handler. *)

val add_service :
  ?middlewares:Tiny_httpd.Middleware.t list ->
  ?prefix:string option ->
  Tiny_httpd.t ->
  handler Pbrt_services.Server.t ->
  unit
(** [add_service http_server service] adds all handlers of [service]
    to the given httpd.

    As a reminder, a RPC "foo" under service "bar" is routed under [prefix/bar/foo]
    using the POST method.
    The body is either binary, alongside the header "application/protobuf",
    or JSON, alongside the header "application/json".
    Errors are always encoded as a JSON object and are indicated by a
    failing HTTP code (outside of [200…299] range).

    RPCs that use streaming (on either the client or server side)
    will always return "501 unimplemented", because
    twirp over http 1.1 does not support streaming.
*)
