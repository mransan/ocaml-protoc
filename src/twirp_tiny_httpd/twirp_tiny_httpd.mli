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
