module H = Tiny_httpd
module PB_server = Pbrt_services.Server
module Twirp_error = Twirp_error
module Error_codes = Error_codes

let spf = Printf.sprintf

exception Fail of Error_codes.t * string option

let fail_ ?msg err = raise (Fail (err, msg))
let failf_ err fmt = Format.kasprintf (fun m -> fail_ err ~msg:m) fmt

let return_error (err : Error_codes.t) (msg : string option) : H.Response.t =
  let msg =
    match msg with
    | Some m -> m
    | None -> Error_codes.to_descr err
  in
  let code, http_code = Error_codes.to_msg_and_code err in
  let err = Twirp_error.default_error ~code ~msg () in
  let json_body : string =
    Twirp_error.encode_json_error err |> Yojson.Basic.to_string
  in
  H.Response.make_raw
    ~headers:[ "content-type", "application/json" ]
    ~code:http_code json_body

let handle_rpc (rpc : PB_server.any_rpc) (req : string H.Request.t) :
    H.Response.t =
  try
    let (PB_server.RPC
          {
            name = _;
            f = handler;
            encode_json_res;
            encode_pb_res;
            decode_json_req;
            decode_pb_req;
          }) =
      rpc
    in

    (* get the raw unary wrapper *)
    let handler : _ -> _ =
      match handler with
      | Unary f -> f
      | _ ->
        failf_ Error_codes.Unimplemented
          "twirp over http 1.1 does not handle streaming"
    in

    let content_type =
      match H.Request.get_header req "content-type" with
      | Some "application/json" -> `JSON
      | Some "application/protobuf" -> `BINARY
      | Some r -> failf_ Error_codes.Malformed "unknown application type %S" r
      | None -> failf_ Error_codes.Malformed "no application type specified"
    in

    (* parse request *)
    let req =
      match content_type with
      | `JSON ->
        (try decode_json_req (Yojson.Basic.from_string req.body)
         with _ -> failf_ Error_codes.Malformed "could not decode json")
      | `BINARY ->
        let dec = Pbrt.Decoder.of_string req.body in
        (try decode_pb_req dec
         with _ -> failf_ Error_codes.Malformed "could not decode protobuf")
    in

    (* call handler *)
    let res =
      try handler req
      with exn ->
        failf_ Error_codes.Internal "handler failed with %s"
          (Printexc.to_string exn)
    in

    (* serialize result *)
    let res =
      match content_type with
      | `JSON -> Yojson.Basic.to_string @@ encode_json_res res
      | `BINARY ->
        let enc = Pbrt.Encoder.create () in
        encode_pb_res res enc;
        Pbrt.Encoder.to_string enc
    in
    H.Response.make_string @@ Ok res
  with
  | Fail (err, msg) -> return_error err msg
  | exn ->
    return_error Error_codes.Unknown
      (Some (spf "handler failed with %s" (Printexc.to_string exn)))

let add_service ?middlewares ?(prefix = Some "twirp") (server : H.t)
    (service : Pbrt_services.Server.t) : unit =
  let add_rpc rpc : unit =
    (* routing is done via:
       [POST [<prefix>]/[<package>.]<Service>/<Method>],
       see {{:https://twitchtv.github.io/twirp/docs/routing.html} the docs}.

       Errors: [https://twitchtv.github.io/twirp/docs/errors.html]
    *)

    (* the [<package>.<Service>] part. *)
    let qualified_service_path_component =
      match service.package with
      | [] -> service.service_name
      | path -> spf "%s.%s" (String.concat "." path) service.service_name
    in

    let route =
      let (PB_server.RPC { name; _ }) = rpc in
      H.Route.(exact qualified_service_path_component @/ exact name @/ return)
    in

    let route =
      match prefix with
      | Some p -> H.Route.(exact p @/ route)
      | None -> route
    in

    H.add_route_handler server ~meth:`POST ?middlewares route @@ fun req ->
    handle_rpc rpc req
  in

  List.iter add_rpc service.handlers
