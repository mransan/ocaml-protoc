(** Runtime for Protobuf services. *)

(** Whether there's a single value or a stream of them *)
module Value_mode = struct
  type unary
  type stream
end

module Push_stream = Push_stream

(** Service stubs, client side *)
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
    encode_pb_req: 'req -> Pbrt.Encode_visitor.t -> unit;
    decode_json_res: Yojson.Basic.t -> 'res;
    decode_pb_res: Pbrt.Decoder.t -> 'res;
  }
  (** A RPC description. You need a transport library
      that knows where to send the bytes to actually use it. *)

  let mk_rpc :
      ?package:string list ->
      service_name:string ->
      rpc_name:string ->
      req_mode:'req_mode mode ->
      res_mode:'res_mode mode ->
      encode_json_req:('req -> Yojson.Basic.t) ->
      encode_pb_req:('req -> Pbrt.Encode_visitor.t -> unit) ->
      decode_json_res:(Yojson.Basic.t -> 'res) ->
      decode_pb_res:(Pbrt.Decoder.t -> 'res) ->
      unit ->
      ('req, 'req_mode, 'res, 'res_mode) rpc =
   fun ?(package = []) ~service_name ~rpc_name ~req_mode ~res_mode
       ~encode_json_req ~encode_pb_req ~decode_json_res ~decode_pb_res () :
       _ rpc ->
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

(** Service stubs, server side *)
module Server = struct
  type 'm mode = 'm Client.mode =
    | Unary : Value_mode.unary mode
    | Stream : Value_mode.stream mode

  type ('req, 'req_mode, 'res, 'res_mode) rpc = {
    name: string;
    req_mode: 'req_mode mode;
    res_mode: 'res_mode mode;
    encode_json_res: 'res -> Yojson.Basic.t;
    encode_pb_res: 'res -> Pbrt.Encode_visitor.t -> unit;
    decode_json_req: Yojson.Basic.t -> 'req;
    decode_pb_req: Pbrt.Decoder.t -> 'req;
  }
  (** A single RPC method, alongside encoders and decoders for
        input and output types. . *)

  (** A RPC endpoint. *)
  type any_rpc = RPC : ('req, 'req_mode, 'res, 'res_mode) rpc -> any_rpc
  [@@unboxed]

  (** Helper to build a RPC *)
  let mk_rpc :
      name:string ->
      req_mode:'req_mode mode ->
      res_mode:'res_mode mode ->
      encode_json_res:('res -> Yojson.Basic.t) ->
      encode_pb_res:('res -> Pbrt.Encode_visitor.t -> unit) ->
      decode_json_req:(Yojson.Basic.t -> 'req) ->
      decode_pb_req:(Pbrt.Decoder.t -> 'req) ->
      unit ->
      ('req, 'req_mode, 'res, 'res_mode) rpc =
   fun ~name ~req_mode ~res_mode ~encode_json_res ~encode_pb_res
       ~decode_json_req ~decode_pb_req () ->
    {
      name;
      req_mode;
      res_mode;
      decode_pb_req;
      decode_json_req;
      encode_pb_res;
      encode_json_res;
    }

  type 'h t = {
    service_name: string;  (** Name of the service *)
    package: string list;
        (** The package this belongs in (e.g. "bigco.auth.secretpasswordstash"),
         split along "." *)
    handlers: 'h list;  (** A list of handlers *)
  }
  (** A service with fixed set of methods, which depends on the concrete RPC
      implementation. Each method is a handler of some type ['h]. *)
end
