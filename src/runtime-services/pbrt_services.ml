(** Client end of services *)
module Client = struct
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

  type ('req, 'ret) rpc = {
    call:
      'actual_ret.
      [ `JSON | `BINARY ] ->
      transport ->
      'req ->
      on_result:(('ret, error) result -> 'actual_ret) ->
      'actual_ret;
  }

  let mk_rpc ~service_name ~rpc_name ~encode_json_req ~encode_pb_req
      ~decode_json_res ~decode_pb_res () : _ rpc =
    {
      call =
        (fun encoding (transport : transport) req ~on_result ->
          let req_str =
            match encoding with
            | `JSON -> encode_json_req req |> Yojson.Safe.to_string
            | `BINARY ->
              let enc = Pbrt.Encoder.create () in
              encode_pb_req req enc;
              Pbrt.Encoder.to_string enc
          in

          let on_result = function
            | Error err -> on_result (Error err)
            | Ok res_str ->
              (match encoding with
              | `JSON ->
                (match decode_json_res @@ Yojson.Safe.from_string res_str with
                | res -> on_result (Ok res)
                | exception exn ->
                  on_result (Error (Decode_error_json (Printexc.to_string exn))))
              | `BINARY ->
                let dec = Pbrt.Decoder.of_string res_str in
                (match decode_pb_res dec with
                | v -> on_result (Ok v)
                | exception Pbrt.Decoder.Failure err ->
                  on_result (Error (Decode_error_binary err))))
          in

          transport.query ~service_name ~rpc_name encoding req_str ~on_result);
    }
end

(** Server end of services *)
module Server = struct
  type error =
    | Invalid_json
    | Invalid_pb of Pbrt.Decoder.error
    | Handler_failed of string

  type rpc = {
    rpc_name: string;
    rpc_handler: [ `JSON | `BINARY ] -> string -> (string, error) result;
  }

  let mk_rpc ~name ~(f : 'req -> 'res) ~encode_json_res ~encode_pb_res
      ~decode_json_req ~decode_pb_req () : rpc =
    let handler fmt req : _ result =
      match fmt with
      | `JSON ->
        (match Yojson.Safe.from_string req with
        | exception _ -> Error Invalid_json
        | j ->
          let req = decode_json_req j in
          (match f req with
          | res -> Ok (encode_json_res res)
          | exception exn -> Error (Handler_failed (Printexc.to_string exn))))
      | `BINARY ->
        let decoder = Pbrt.Decoder.of_string req in
        (match decode_pb_req decoder with
        | exception Pbrt.Decoder.Failure e -> Error (Invalid_pb e)
        | req ->
          (match f req with
          | res ->
            let enc = Pbrt.Encoder.create () in
            encode_pb_res res enc;
            Ok (Pbrt.Encoder.to_string enc)
          | exception exn -> Error (Handler_failed (Printexc.to_string exn))))
    in

    { rpc_name = name; rpc_handler = handler }

  type t = {
    name: string;
    handlers: rpc list;
  }
end
