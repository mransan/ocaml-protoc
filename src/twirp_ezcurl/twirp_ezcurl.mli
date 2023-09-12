module Twirp_error = Twirp_error
open Pbrt_services

type error = Twirp_error.error

val pp_error : Format.formatter -> error -> unit

val call :
  ?client:Ezcurl.t ->
  ?encoding:[ `JSON | `BINARY ] ->
  ?prefix:string option ->
  ?use_tls:bool ->
  host:string ->
  port:int ->
  ('req, Value_mode.unary, 'res, Value_mode.unary) Client.rpc ->
  'req ->
  ('res, error) result

exception E_twirp of error

val call_exn :
  ?client:Ezcurl.t ->
  ?encoding:[ `JSON | `BINARY ] ->
  ?prefix:string option ->
  ?use_tls:bool ->
  host:string ->
  port:int ->
  ('req, Value_mode.unary, 'res, Value_mode.unary) Client.rpc ->
  'req ->
  'res
(** Same as {!call} but raises [E_twirp] on failure. *)
