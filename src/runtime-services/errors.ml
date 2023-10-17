[@@@ocaml.warning "-27-30-39"]

type empty = unit

type timeout_info = {
  timeout_s : float;
}

type rpc_error =
  | Invalid_binary of string
  | Invalid_json of string
  | Timeout of timeout_info
  | Server_error of string
  | Transport_error of string
  | Unknown_error

let rec default_empty = ()

let rec default_timeout_info 
  ?timeout_s:((timeout_s:float) = 0.)
  () : timeout_info  = {
  timeout_s;
}

let rec default_rpc_error () : rpc_error = Invalid_binary ("")

type timeout_info_mutable = {
  mutable timeout_s : float;
}

let default_timeout_info_mutable () : timeout_info_mutable = {
  timeout_s = 0.;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_empty fmt (v:empty) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_unit fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_timeout_info fmt (v:timeout_info) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "timeout_s" Pbrt.Pp.pp_float fmt v.timeout_s;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_rpc_error fmt (v:rpc_error) =
  match v with
  | Invalid_binary x -> Format.fprintf fmt "@[<hv2>Invalid_binary(@,%a)@]" Pbrt.Pp.pp_string x
  | Invalid_json x -> Format.fprintf fmt "@[<hv2>Invalid_json(@,%a)@]" Pbrt.Pp.pp_string x
  | Timeout x -> Format.fprintf fmt "@[<hv2>Timeout(@,%a)@]" pp_timeout_info x
  | Server_error x -> Format.fprintf fmt "@[<hv2>Server_error(@,%a)@]" Pbrt.Pp.pp_string x
  | Transport_error x -> Format.fprintf fmt "@[<hv2>Transport_error(@,%a)@]" Pbrt.Pp.pp_string x
  | Unknown_error  -> Format.fprintf fmt "Unknown_error"

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_empty (v:empty) encoder = 
()

let rec encode_pb_timeout_info (v:timeout_info) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.timeout_s encoder;
  ()

let rec encode_pb_rpc_error (v:rpc_error) encoder = 
  begin match v with
  | Invalid_binary x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Invalid_json x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Timeout x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_pb_timeout_info x) encoder;
  | Server_error x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Transport_error x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Unknown_error ->
    Pbrt.Encoder.key (0, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  end

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_empty d =
  match Pbrt.Decoder.key d with
  | None -> ();
  | Some (_, pk) -> 
    Pbrt.Decoder.unexpected_payload "Unexpected fields in empty message(empty)" pk

let rec decode_pb_timeout_info d =
  let v = default_timeout_info_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits32) -> begin
      v.timeout_s <- Pbrt.Decoder.float_as_bits32 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(timeout_info), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    timeout_s = v.timeout_s;
  } : timeout_info)

let rec decode_pb_rpc_error d = 
  let rec loop () = 
    let ret:rpc_error = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "rpc_error"
      | Some (5, _) -> (Invalid_binary (Pbrt.Decoder.string d) : rpc_error) 
      | Some (4, _) -> (Invalid_json (Pbrt.Decoder.string d) : rpc_error) 
      | Some (3, _) -> (Timeout (decode_pb_timeout_info (Pbrt.Decoder.nested d)) : rpc_error) 
      | Some (2, _) -> (Server_error (Pbrt.Decoder.string d) : rpc_error) 
      | Some (1, _) -> (Transport_error (Pbrt.Decoder.string d) : rpc_error) 
      | Some (0, _) -> begin 
        Pbrt.Decoder.empty_nested d ;
        (Unknown_error : rpc_error)
      end
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_empty (v:empty) = 
Pbrt_yojson.make_unit v

let rec encode_json_timeout_info (v:timeout_info) = 
  let assoc = [] in 
  let assoc = ("timeoutS", Pbrt_yojson.make_float v.timeout_s) :: assoc in
  `Assoc assoc

let rec encode_json_rpc_error (v:rpc_error) = 
  begin match v with
  | Invalid_binary v -> `Assoc [("invalidBinary", Pbrt_yojson.make_string v)]
  | Invalid_json v -> `Assoc [("invalidJson", Pbrt_yojson.make_string v)]
  | Timeout v -> `Assoc [("timeout", encode_json_timeout_info v)]
  | Server_error v -> `Assoc [("serverError", Pbrt_yojson.make_string v)]
  | Transport_error v -> `Assoc [("transportError", Pbrt_yojson.make_string v)]
  | Unknown_error -> `Assoc [("unknownError", `Null)]
  end

[@@@ocaml.warning "-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_empty d =
Pbrt_yojson.unit d "empty" "empty record"

let rec decode_json_timeout_info d =
  let v = default_timeout_info_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("timeoutS", json_value) -> 
      v.timeout_s <- Pbrt_yojson.float json_value "timeout_info" "timeout_s"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    timeout_s = v.timeout_s;
  } : timeout_info)

let rec decode_json_rpc_error json =
  let assoc = match json with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  let rec loop = function
    | [] -> Pbrt_yojson.E.malformed_variant "rpc_error"
    | ("invalidBinary", json_value)::_ -> 
      (Invalid_binary (Pbrt_yojson.string json_value "rpc_error" "Invalid_binary") : rpc_error)
    | ("invalidJson", json_value)::_ -> 
      (Invalid_json (Pbrt_yojson.string json_value "rpc_error" "Invalid_json") : rpc_error)
    | ("timeout", json_value)::_ -> 
      (Timeout ((decode_json_timeout_info json_value)) : rpc_error)
    | ("serverError", json_value)::_ -> 
      (Server_error (Pbrt_yojson.string json_value "rpc_error" "Server_error") : rpc_error)
    | ("transportError", json_value)::_ -> 
      (Transport_error (Pbrt_yojson.string json_value "rpc_error" "Transport_error") : rpc_error)
    | ("unknownError", _)::_-> (Unknown_error : rpc_error)
    
    | _ :: tl -> loop tl
  in
  loop assoc
