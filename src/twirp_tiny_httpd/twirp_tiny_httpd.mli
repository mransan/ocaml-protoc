(** Twirp for tiny_httpd.

    This implements the server end of https://github.com/twitchtv/twirp *)

module Twirp_error = Twirp_error
module Error_codes = Error_codes

val add_service :
  ?middlewares:Tiny_httpd.Middleware.t list ->
  ?prefix:string option ->
  Tiny_httpd.t ->
  Pbrt_services.Server.t ->
  unit
(** [add_service http_server service] adds all handlers of [service]
    to the given httpd.

    Services that use streaming will return "501 unimplemented", because
    twirp over http 1.1 does not support streaming. *)
