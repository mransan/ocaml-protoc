module Errors = Errors

type rpc_error = Errors.rpc_error =
  | Invalid_binary of string
  | Invalid_json of string
  | Timeout of Errors.timeout_info
  | Server_error of string
  | Transport_error of string
  | Unknown_error

let pp_rpc_error = Errors.pp_rpc_error

(** Client end of services *)
module Client = struct
  type ('req, 'ret) rpc = {
    service_name: string;
    rpc_name: string;
    encode_json_req: 'req -> Yojson.Basic.t;
    encode_pb_req: 'req -> Pbrt.Encoder.t -> unit;
    decode_json_res: Yojson.Basic.t -> 'ret;
    decode_pb_res: Pbrt.Decoder.t -> 'ret;
  }

  let mk_rpc ~service_name ~rpc_name ~encode_json_req ~encode_pb_req
      ~decode_json_res ~decode_pb_res () : _ rpc =
    {
      service_name;
      rpc_name;
      encode_pb_req;
      encode_json_req;
      decode_pb_res;
      decode_json_res;
    }
end

(** Server end of services *)
module Server = struct
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

  let mk_rpc ~name ~(f : 'req -> 'res) ~encode_json_res ~encode_pb_res
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
    handlers: rpc list;
  }
end
