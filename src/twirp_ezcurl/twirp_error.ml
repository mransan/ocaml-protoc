[@@@ocaml.warning "-27-30-39"]

type error = {
  code : string;
  msg : string;
}

let rec default_error 
  ?code:((code:string) = "")
  ?msg:((msg:string) = "")
  () : error  = {
  code;
  msg;
}

type error_mutable = {
  mutable code : string;
  mutable msg : string;
}

let default_error_mutable () : error_mutable = {
  code = "";
  msg = "";
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_error fmt (v:error) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "code" Pbrt.Pp.pp_string fmt v.code;
    Pbrt.Pp.pp_record_field ~first:false "msg" Pbrt.Pp.pp_string fmt v.msg;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_error (v:error) = 
  let assoc = [] in 
  let assoc = ("code", Pbrt_yojson.make_string v.code) :: assoc in
  let assoc = ("msg", Pbrt_yojson.make_string v.msg) :: assoc in
  `Assoc assoc

[@@@ocaml.warning "-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_error d =
  let v = default_error_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("code", json_value) -> 
      v.code <- Pbrt_yojson.string json_value "error" "code"
    | ("msg", json_value) -> 
      v.msg <- Pbrt_yojson.string json_value "error" "msg"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    code = v.code;
    msg = v.msg;
  } : error)
