module Errors = Errors

type rpc_error = Errors.rpc_error =
  | Invalid_binary of string
  | Invalid_json of string
  | Timeout of Errors.timeout_info
  | Server_error of string
  | Transport_error of string
  | Unknown_error

let pp_rpc_error = Errors.pp_rpc_error

module Value_mode = struct
  type unary
  type stream
end

module Pull_stream = struct
  type 'a t = { pull: 'ret. unit -> on_result:('a option -> 'ret) -> unit }
end

module Push_stream = struct
  type 'a t = {
    push: 'a -> unit;
    close: unit -> unit;
  }

  let push self x = self.push x
  let close self = self.close ()
end

(** Client end of services *)
module Client = struct
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

  let mk_rpc ?(package = []) ~service_name ~rpc_name ~req_mode ~res_mode
      ~encode_json_req ~encode_pb_req ~decode_json_res ~decode_pb_res () : _ rpc
      =
    {
      service_name;
      package;
      rpc_name;
      req_mode;
      res_mode;
      encode_pb_req;
      encode_json_req;
      decode_pb_res;
      decode_json_res;
    }
end

(** Server end of services *)
module Server = struct
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

  let mk_rpc ~name ~(f : _ handler) ~encode_json_res ~encode_pb_res
      ~decode_json_req ~decode_pb_req () : rpc =
    RPC
      {
        name;
        f;
        decode_pb_req;
        decode_json_req;
        encode_pb_res;
        encode_json_res;
      }

  type t = {
    service_name: string;
    package: string list;
    handlers: rpc list;
  }
end
