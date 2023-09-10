module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let string_of_rpc_type (ty : Ot.rpc_type) : string =
  let f = Pb_codegen_util.string_of_field_type in
  match ty with
  | Ot.Rpc_scalar ty -> f ty
  | Ot.Rpc_stream ty -> Printf.sprintf "%s Seq.t" (f ty)

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
  | Ot.Rpc_scalar ty -> f ty
  | Ot.Rpc_stream ty -> Printf.sprintf "Seq.map %s" (f ty)

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
  | Ot.Rpc_scalar ty -> f ty
  | Ot.Rpc_stream ty -> Printf.sprintf "Seq.map %s" (f ty)

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
  | Ot.Rpc_scalar ty -> f ty
  | Ot.Rpc_stream ty -> Printf.sprintf "Seq.map %s" (f ty)

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
  | Ot.Rpc_scalar ty -> f ty
  | Ot.Rpc_stream ty -> Printf.sprintf "Seq.map %s" (f ty)

let ocaml_type_of_rpc_type (rpc : Ot.rpc_type) : string =
  match rpc with
  | Rpc_scalar ty -> Pb_codegen_util.string_of_field_type ty
  | Rpc_stream ty ->
    Printf.sprintf "(%s Seq.t)" (Pb_codegen_util.string_of_field_type ty)

let mod_name_for_client (service : Ot.service) : string =
  String.capitalize_ascii service.service_name

let gen_service_client_struct (service : Ot.service) sc : unit =
  let service_name = service.service_name in
  F.linep sc "module %s = struct" (mod_name_for_client service);
  F.sub_scope sc (fun sc ->
      F.linep sc "open Pbrt_services.Client";
      List.iter
        (fun (rpc : Ot.rpc) ->
          let rpc_name = rpc.rpc_name in
          F.empty_line sc;
          F.linep sc "let %s : (%s, %s) rpc ="
            (Pb_codegen_util.function_name_of_rpc rpc)
            (string_of_rpc_type rpc.rpc_req)
            (string_of_rpc_type rpc.rpc_res);
          F.linep sc " (mk_rpc ~service_name:%S ~rpc_name:%S"
            service.service_name rpc.rpc_name;
          F.linep sc "    ~encode_json_req:%s"
            (function_name_encode_json ~service_name ~rpc_name rpc.rpc_req);
          F.linep sc "    ~encode_pb_req:%s"
            (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_req);
          F.linep sc "    ~decode_json_res:%s"
            (function_name_decode_json ~service_name ~rpc_name rpc.rpc_res);
          F.linep sc "    ~decode_pb_res:%s"
            (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_res);
          F.linep sc "() : (%s, %s) rpc)"
            (ocaml_type_of_rpc_type rpc.rpc_req)
            (ocaml_type_of_rpc_type rpc.rpc_res))
        service.service_body);

  F.line sc "end";
  F.empty_line sc

let gen_service_client_sig (service : Ot.service) sc : unit =
  F.linep sc "(** Client for %s *)" service.service_name;
  F.linep sc "module %s : sig" (mod_name_for_client service);
  F.sub_scope sc (fun sc ->
      F.linep sc "open Pbrt_services.Client";
      List.iter
        (fun (rpc : Ot.rpc) ->
          F.empty_line sc;
          F.linep sc "val %s : (%s, %s) rpc"
            (Pb_codegen_util.function_name_of_rpc rpc)
            (string_of_rpc_type rpc.rpc_req)
            (string_of_rpc_type rpc.rpc_res))
        service.service_body);
  F.line sc "end";
  F.empty_line sc

(** generate the module type for the server (shared between .ml and .mli) *)
let gen_mod_type_of_service (service : Ot.service) sc : unit =
  let mod_type_name =
    Pb_codegen_util.module_type_name_of_service_server service
  in

  F.linep sc "module type %s = sig" mod_type_name;
  F.sub_scope sc (fun sc ->
      List.iter
        (fun (rpc : Ot.rpc) ->
          F.linep sc "val %s : %s -> %s"
            (Pb_codegen_util.function_name_of_rpc rpc)
            (string_of_rpc_type rpc.rpc_req)
            (string_of_rpc_type rpc.rpc_res))
        service.service_body);
  F.line sc "end"

let gen_service_server_struct (service : Ot.service) sc : unit =
  let service_name = service.service_name in
  let mod_type_name =
    Pb_codegen_util.module_type_name_of_service_server service
  in

  gen_mod_type_of_service service sc;
  F.empty_line sc;

  (* now generate a function from the module type to a [Service_server.t] *)
  F.linep sc "let service_impl_of_%s (module M:%s) : Pbrt_services.Server.t ="
    (String.lowercase_ascii service_name)
    mod_type_name;
  F.sub_scope sc (fun sc ->
      F.line sc "let open Pbrt_services.Server in";
      F.linep sc "{ name=%S;" service_name;
      F.line sc "  handlers=[";
      List.iter
        (fun (rpc : Ot.rpc) ->
          let rpc_name = rpc.rpc_name in
          F.linep sc "   (mk_rpc ~name:%S ~f:M.%s" rpc.rpc_name
            (Pb_codegen_util.function_name_of_rpc rpc);
          F.linep sc "      ~encode_json_res:%s"
            (function_name_encode_json ~service_name ~rpc_name rpc.rpc_res);
          F.linep sc "      ~encode_pb_res:%s"
            (function_name_encode_pb ~service_name ~rpc_name rpc.rpc_res);
          F.linep sc "      ~decode_json_req:%s"
            (function_name_decode_json ~service_name ~rpc_name rpc.rpc_req);
          F.linep sc "      ~decode_pb_req:%s"
            (function_name_decode_pb ~service_name ~rpc_name rpc.rpc_req);
          F.linep sc "      () : rpc);")
        service.service_body;
      F.line sc "]; }");
  F.empty_line sc

let gen_service_server_sig service sc : unit =
  let mod_type_name =
    Pb_codegen_util.module_type_name_of_service_server service
  in

  F.linep sc "(** Server interface for %s *)" service.service_name;
  gen_mod_type_of_service service sc;
  F.empty_line sc;

  F.linep sc "(** Convert {!%s} to a generic runtime service *)" mod_type_name;
  F.linep sc "val service_impl_of_%s : (module %s) -> Pbrt_services.Server.t"
    (String.lowercase_ascii service.service_name)
    mod_type_name;
  ()
