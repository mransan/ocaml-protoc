module Ot = Pb_codegen_ocaml_type
module F = Pb_codegen_formatting

let spf = Printf.sprintf

let string_of_rpc_type (ty : Ot.rpc_type) : string =
  let f = Pb_codegen_util.string_of_field_type in
  match ty with
  | Ot.Rpc_scalar ty -> f ty
  | Ot.Rpc_stream ty -> Printf.sprintf "%s Seq.t" (f ty)

let decode_json_ty ~r_name ~rf_label (ty : Ot.rpc_type) : string =
  let f ty =
    let x, body =
      Pb_codegen_decode_yojson.field_pattern_match ~r_name ~rf_label ty
    in
    spf "(fun %s -> %s)" x body
  in
  match ty with
  | Ot.Rpc_scalar ty -> f ty
  | Ot.Rpc_stream ty -> Printf.sprintf "Seq.map %s" (f ty)

let mod_name_for_client (service : Ot.service) : string =
  String.capitalize_ascii service.service_name

let gen_service_client_struct (service : Ot.service) sc : unit =
  F.linep sc "module %s = struct" (mod_name_for_client service);
  F.sub_scope sc (fun sc ->
      F.linep sc "open Pbrt_services.Client";
      List.iter
        (fun (rpc : Ot.rpc) ->
          F.empty_line sc;
          F.linep sc "let %s : (%s, %s) rpc ="
            (Pb_codegen_util.function_name_of_rpc rpc)
            (string_of_rpc_type rpc.rpc_req)
            (string_of_rpc_type rpc.rpc_res);
          F.linep sc "  mk_rpc ~service_name:%S ~rpc_name:%S"
            service.service_name rpc.rpc_name;
          F.linep sc "    ~encode_json_req:encode_json_%s"
            (string_of_rpc_type rpc.rpc_req);
          F.linep sc "    ~encode_pb_req:encode_pb_%s"
            (string_of_rpc_type rpc.rpc_req);
          F.linep sc "    ~decode_json_res:%s"
            (decode_json_ty ~r_name:service.service_name ~rf_label:rpc.rpc_name
               rpc.rpc_res);
          F.linep sc "    ~decode_pb_res:decode_pb_%s"
            (string_of_rpc_type rpc.rpc_res);
          F.line sc "()")
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
  let mod_type_name =
    Pb_codegen_util.module_type_name_of_service_server service
  in

  gen_mod_type_of_service service sc;
  F.empty_line sc;

  (* now generate a function from the module type to a [Service_server.t] *)
  F.linep sc "let service_impl_of_%s (module M:%s) : Pbrt_services.Server.t ="
    (String.lowercase_ascii service.service_name)
    mod_type_name;
  F.sub_scope sc (fun sc ->
      F.line sc "let open Pbrt_services.Server in";
      F.linep sc "{ name=%S;" service.service_name;
      F.line sc "  handlers=[";
      List.iter
        (fun (rpc : Ot.rpc) ->
          F.linep sc "    mk_rpc ~name:%S ~f:M.%s" rpc.rpc_name
            (Pb_codegen_util.function_name_of_rpc rpc);
          F.linep sc "      ~encode_json_res:encode_json_%s"
            (string_of_rpc_type rpc.rpc_res);
          F.linep sc "      ~encode_pb_res:encode_pb_%s"
            (string_of_rpc_type rpc.rpc_res);
          F.linep sc "      ~decode_json_req:(%s)"
            (decode_json_ty ~r_name:service.service_name ~rf_label:rpc.rpc_name
               rpc.rpc_req);
          F.linep sc "      ~decode_pb_req:decode_pb_%s"
            (string_of_rpc_type rpc.rpc_req);
          F.linep sc "      ();")
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
