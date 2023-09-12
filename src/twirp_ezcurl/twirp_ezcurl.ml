open Pbrt_services
module Twirp_error = Twirp_error

let spf = Printf.sprintf

type error = Twirp_error.error

let pp_error = Twirp_error.pp_error

let decode_error exn : error =
  {
    Twirp_error.code = "decoding error";
    msg = spf "decoding response failed with: %s" (Printexc.to_string exn);
  }

let unknown_error msg : error =
  {
    Twirp_error.code = "unknown";
    msg = spf "call failed with unknown reason: %s" msg;
  }

let call ?(client : Ezcurl.t = Ezcurl.make ())
    ?(encoding : [ `JSON | `BINARY ] = `BINARY) ?(prefix = Some "twirp")
    ?(use_tls = true) ~host ~port
    (rpc : ('req, Value_mode.unary, 'res, Value_mode.unary) Client.rpc)
    (req : 'req) : ('res, error) result =
  (* first, encode query *)
  let (req_data : string), content_type =
    match encoding with
    | `JSON ->
      let data = rpc.encode_json_req req |> Yojson.Basic.to_string in
      data, "application/json"
    | `BINARY ->
      let enc = Pbrt.Encoder.create () in
      rpc.encode_pb_req req enc;
      Pbrt.Encoder.to_string enc, "application/protobuf"
  in

  (* Compute remote URL.
     Routing is done via:
     [POST [<prefix>]/[<package>.]<Service>/<Method>],
     see {{:https://twitchtv.github.io/twirp/docs/routing.html} the docs}.

     Errors: [https://twitchtv.github.io/twirp/docs/errors.html]
  *)
  let url : string =
    (* the [<package>.<Service>] part. *)
    let qualified_service_path_component =
      match rpc.package with
      | [] -> rpc.service_name
      | path -> spf "%s.%s" (String.concat "." path) rpc.service_name
    in

    let prefix =
      match prefix with
      | None -> ""
      | Some p -> spf "%s/" p
    in

    let protocol =
      if use_tls then
        "https"
      else
        "http"
    in
    spf "%s://%s:%d/%s%s/%s" protocol host port prefix
      qualified_service_path_component rpc.rpc_name
  in

  Printf.printf "url: %S\n" url;

  let headers = [ "content-type", content_type ] in

  let res : _ result =
    Ezcurl.post ~client ~url ~params:[] ~content:(`String req_data) ~headers ()
  in

  match res with
  | Ok { code; body; headers = _; _ } when code >= 200 && code < 300 ->
    (* success *)
    (match
       match encoding with
       | `JSON -> rpc.decode_json_res (Yojson.Basic.from_string body)
       | `BINARY -> rpc.decode_pb_res (Pbrt.Decoder.of_string body)
     with
    | res -> Ok res
    | exception exn -> Error (decode_error exn))
  | Ok { body; headers = _; _ } ->
    (match Twirp_error.decode_json_error @@ Yojson.Basic.from_string body with
    | err -> Error err
    | exception exn -> Error (decode_error exn))
  | Error _ -> Error (unknown_error "http call failed")

exception E_twirp of error

let call_exn ?client ?encoding ?prefix ?use_tls ~host ~port rpc req =
  match call ?client ?encoding ?prefix ?use_tls ~host ~port rpc req with
  | Ok x -> x
  | Error err -> raise (E_twirp err)
