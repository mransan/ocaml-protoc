module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let spf = Printf.sprintf

let ocaml_type_of_rpc_type (rpc : Ot.rpc_type) : string * string =
  match rpc with
  | Rpc_scalar ty -> Pb_codegen_util.string_of_field_type ty, "unary"
  | Rpc_stream ty -> Pb_codegen_util.string_of_field_type ty, "stream"

let string_of_server_rpc (req : Ot.rpc_type) (res : Ot.rpc_type) : string =
  let req, req_mode = ocaml_type_of_rpc_type req in
  let res, res_mode = ocaml_type_of_rpc_type res in
  spf "(%s, %s, %s, %s) Server.rpc" req req_mode res res_mode

let function_name_encode_json ~service_name ~rpc_name (ty : Ot.rpc_type) :
    string =
  let f ty =
    match ty with
    | Ot.Ft_unit -> "(fun () -> `Assoc [])"
    | Ot.Ft_user_defined_type udt ->
      let function_prefix = "encode_json" in
      Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    | _ ->
      Printf.eprintf "cannot json-encode request for %s in service %s\n%!"
        rpc_name service_name;
      exit 1
  in
  match ty with
  | Ot.Rpc_scalar ty | Ot.Rpc_stream ty -> f ty

let function_name_decode_json ~service_name ~rpc_name (ty : Ot.rpc_type) :
    string =
  let f ty =
    match ty with
    | Ot.Ft_unit -> "(fun _ -> ())"
    | Ot.Ft_user_defined_type udt ->
      let function_prefix = "decode_json" in
      Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    | _ ->
      Printf.eprintf "cannot decode json request for %s in service %s\n%!"
        rpc_name service_name;
      exit 1
  in
  match ty with
  | Ot.Rpc_scalar ty | Ot.Rpc_stream ty -> f ty

let function_name_encode_pb ~service_name ~rpc_name (ty : Ot.rpc_type) : string
    =
  let f ty =
    match ty with
    | Ot.Ft_unit -> "(fun () enc -> Pbrt.Encoder.empty_nested enc)"
    | Ot.Ft_user_defined_type udt ->
      let function_prefix = "encode_pb" in
      Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    | _ ->
      Printf.eprintf "cannot binary-encode request for %s in service %s\n%!"
        rpc_name service_name;
      exit 1
  in
  match ty with
  | Ot.Rpc_scalar ty | Ot.Rpc_stream ty -> f ty

let function_name_decode_pb ~service_name ~rpc_name (ty : Ot.rpc_type) : string
    =
  let f ty =
    match ty with
    | Ot.Ft_unit -> "(fun d -> Pbrt.Decoder.empty_nested d)"
    | Ot.Ft_user_defined_type udt ->
      let function_prefix = "decode_pb" in
      Pb_codegen_util.function_name_of_user_defined ~function_prefix udt
    | _ ->
      Printf.eprintf "cannot decode binary request for %s in service %s\n%!"
        rpc_name service_name;
      exit 1
  in
  match ty with
  | Ot.Rpc_scalar ty | Ot.Rpc_stream ty -> f ty

let mod_name_for_client (service : Ot.service) : string =
  String.capitalize_ascii service.service_name

let string_list_of_package (path : string list) : string =
  spf "[%s]" (String.concat ";" @@ List.map (fun s -> spf "%S" s) path)

let gen_service_client_struct (service : Ot.service) sc : unit =
  let service_name = service.service_name in
  F.line sc "module Client = struct";
  let gen_rpc sc (rpc : Ot.rpc) =
    F.linep sc "open Pbrt_services";
    let rpc_name = rpc.rpc_name in
    let req, req_mode = ocaml_type_of_rpc_type rpc.rpc_req in
    let req_mode_witness = String.capitalize_ascii req_mode in
    let res, res_mode = ocaml_type_of_rpc_type rpc.rpc_res in
    let res_mode_witness = String.capitalize_ascii res_mode in
    F.empty_line sc;
    F.linep sc "let %s : (%s, %s, %s, %s) Client.rpc ="
      (Pb_codegen_util.function_name_of_rpc rpc)
      req req_mode res res_mode;
    F.linep sc "  (Client.mk_rpc ";
    F.linep sc "    ~package:%s"
      (string_list_of_package service.service_packages);
    F.linep sc "    ~service_name:%S ~rpc_name:%S" service.service_name
      rpc.rpc_name;
    F.linep sc "    ~req_mode:Client.%s" req_mode_witness;
    F.linep sc "    ~res_mode:Client.%s" res_mode_witness;
    F.linep sc "    ~encode_json_req:%s"
      (function_name_encode_json ~service_name ~rpc_name rpc.rpc_req);
    F.linep sc "    ~encode_pb_req:%s"
      (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req);
    F.linep sc "    ~decode_json_res:%s"
      (function_name_decode_json ~service_name ~rpc_name rpc.rpc_res);
    F.linep sc "    ~decode_pb_res:%s"
      (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res);
    let req, req_mode = ocaml_type_of_rpc_type rpc.rpc_req in
    let res, res_mode = ocaml_type_of_rpc_type rpc.rpc_res in
    F.linep sc "    () : (%s, %s, %s, %s) Client.rpc)" req req_mode res res_mode
  in
  F.sub_scope sc (fun sc -> List.iter (gen_rpc sc) service.service_body);
  F.line sc "end"

let gen_service_server_struct (service : Ot.service) sc : unit =
  let service_name = service.service_name in

  (* generate rpc descriptions for the server side *)
  let gen_rpc sc (rpc : Ot.rpc) =
    F.empty_line sc;
    let rpc_name = rpc.rpc_name in
    let name = Pb_codegen_util.function_name_of_rpc rpc in
    let req, req_mode = ocaml_type_of_rpc_type rpc.rpc_req in
    let res, res_mode = ocaml_type_of_rpc_type rpc.rpc_res in
    let req_mode_witness = String.capitalize_ascii req_mode in
    let res_mode_witness = String.capitalize_ascii res_mode in

    F.linep sc "let %s : (%s,%s,%s,%s) Server.rpc = " name req req_mode res
      res_mode;
    F.linep sc "  (Server.mk_rpc ~name:%S" rpc.rpc_name;
    F.linep sc "    ~req_mode:Server.%s" req_mode_witness;
    F.linep sc "    ~res_mode:Server.%s" res_mode_witness;
    F.linep sc "    ~encode_json_res:%s"
      (function_name_encode_json ~service_name ~rpc_name rpc.rpc_res);
    F.linep sc "    ~encode_pb_res:%s"
      (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_res);
    F.linep sc "    ~decode_json_req:%s"
      (function_name_decode_json ~service_name ~rpc_name rpc.rpc_req);
    F.linep sc "    ~decode_pb_req:%s"
      (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_req);
    F.linep sc "    () : _ Server.rpc)"
  in

  let gen_server sc =
    let rpc_parameter_name name = spf "__handler__%s" name in
    F.line sc "open Pbrt_services";
    List.iter (gen_rpc sc) service.service_body;

    (* now generate a function from the module type to a [Service_server.t] *)
    F.empty_line sc;
    F.linep sc "let make";
    List.iter
      (fun (rpc : Ot.rpc) ->
        let name = Pb_codegen_util.function_name_of_rpc rpc in
        F.linep sc "  ~%s:%s" name (rpc_parameter_name name))
      service.service_body;
    F.line sc "  () : _ Server.t =";
    F.linep sc "  { Server.";
    F.linep sc "    service_name=%S;" service_name;
    F.linep sc "    package=%s;"
      (string_list_of_package service.service_packages);
    F.line sc "    handlers=[";
    List.iter
      (fun (rpc : Ot.rpc) ->
        let name = Pb_codegen_util.function_name_of_rpc rpc in
        F.linep sc "       (%s %s);" (rpc_parameter_name name) name)
      service.service_body;
    F.line sc "    ];";
    F.line sc "  }"
  in

  F.empty_line sc;
  F.line sc "module Server = struct";
  F.sub_scope sc gen_server;
  F.line sc "end";
  F.empty_line sc

let gen_service_struct (service : Ot.service) sc : unit =
  F.linep sc "module %s = struct" (mod_name_for_client service);
  F.sub_scope sc (fun sc ->
      F.linep sc "open Pbrt_services.Value_mode";

      gen_service_client_struct service sc;

      (* now the server side *)
      gen_service_server_struct service sc);

  F.line sc "end";
  F.empty_line sc

let gen_service_sig (service : Ot.service) sc : unit =
  F.linep sc "(** %s service *)" service.service_name;
  F.linep sc "module %s : sig" (mod_name_for_client service);
  F.sub_scope sc (fun sc ->
      F.linep sc "open Pbrt_services";
      F.linep sc "open Pbrt_services.Value_mode";

      (* client *)
      let gen_client_rpc sc (rpc : Ot.rpc) =
        F.empty_line sc;
        let req, req_mode = ocaml_type_of_rpc_type rpc.rpc_req in
        let res, res_mode = ocaml_type_of_rpc_type rpc.rpc_res in
        F.linep sc "val %s : (%s, %s, %s, %s) Client.rpc"
          (Pb_codegen_util.function_name_of_rpc rpc)
          req req_mode res res_mode
      in

      F.empty_line sc;
      F.line sc "module Client : sig";
      F.sub_scope sc (fun sc ->
          List.iter (gen_client_rpc sc) service.service_body);
      F.line sc "end";

      (* server *)
      F.empty_line sc;
      F.line sc "module Server : sig";
      F.sub_scope sc (fun sc ->
          F.line sc "(** Produce a server implementation from handlers *)";
          F.linep sc "val make : ";
          List.iter
            (fun (rpc : Ot.rpc) ->
              F.linep sc "  %s:(%s -> 'handler) ->"
                (Pb_codegen_util.function_name_of_rpc rpc)
                (string_of_server_rpc rpc.rpc_req rpc.rpc_res))
            service.service_body;
          F.linep sc "  unit -> 'handler Pbrt_services.Server.t";

          F.empty_line sc;
          F.line sc
            "(** The individual server stubs are only exposed for advanced \
             users. Casual users should prefer accessing them through {!make}. \
             *)";
          List.iter
            (fun (rpc : Ot.rpc) ->
              F.empty_line sc;
              let name = Pb_codegen_util.function_name_of_rpc rpc in
              let req, req_mode = ocaml_type_of_rpc_type rpc.rpc_req in
              let res, res_mode = ocaml_type_of_rpc_type rpc.rpc_res in
              F.linep sc "val %s : (%s,%s,%s,%s) Server.rpc" name req req_mode
                res res_mode)
            service.service_body);

      F.line sc "end";

      ());

  F.line sc "end";
  F.empty_line sc
