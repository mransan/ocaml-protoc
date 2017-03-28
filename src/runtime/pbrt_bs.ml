module E = struct

  type error = 
    | Unexpected_json_type of string * string  
    | Malformed_variant of string  

  exception Failure of error 

  let unexpected_json_type record_name field_name = 
    raise (Failure (Unexpected_json_type (record_name, field_name)))

  let malformed_variant variant_name = 
    raise (Failure (Malformed_variant variant_name)) 

  let string_of_error = function
    | Unexpected_json_type (record_name, field_name) -> 
      Printf.sprintf "Unexpected json type (record name:%s, field_name:%s)"
        record_name field_name 
    | Malformed_variant variant_name ->
      Printf.sprintf "Malformed variant (variant name: %s)" variant_name

  let () =
    Printexc.register_printer (fun exn ->
      match exn with
      | Failure e -> Some (string_of_error e)
      | _ -> None
      )
  
end 

let int32_ : 
  type a. (a Js_json.kind * a) -> string -> string -> int32 
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.String -> Int32.of_string v 
  | Js_json.Number -> Int32.of_float v 
  | Js_json.Null -> 0l 
  | _ -> E.unexpected_json_type record_name field_name  

let int32 json record_name field_name = 
  int32_ (Js_json.reifyType json) record_name field_name 

let int64_ : 
  type a. (a Js_json.kind * a) -> string -> string -> int64 
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.String -> Int64.of_string v 
  | Js_json.Number -> Int64.of_float v 
  | Js_json.Null -> 0L
  | _ -> E.unexpected_json_type record_name field_name  

let int64 json record_name field_name = 
  int64_ (Js_json.reifyType json) record_name field_name 

let float_ : 
  type a. (a Js_json.kind * a) -> string -> string -> float 
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.String -> float_of_string v 
  | Js_json.Number -> v 
  | Js_json.Null -> 0.0
  | _ -> E.unexpected_json_type record_name field_name  

let float json record_name field_name = 
  float_ (Js_json.reifyType json) record_name field_name 

let int_ : 
  type a. (a Js_json.kind * a) -> string -> string -> int 
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.String -> int_of_string v 
  | Js_json.Number -> int_of_float v 
  | Js_json.Null -> 0
  | _ -> E.unexpected_json_type record_name field_name  

let int json record_name field_name = 
  int_ (Js_json.reifyType json) record_name field_name 

let bool_ : 
  type a. (a Js_json.kind * a) -> string -> string -> bool 
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.Boolean -> Js.to_bool v 
  | Js_json.Null -> false
  | _ -> E.unexpected_json_type record_name field_name  

let bool json record_name field_name = 
  bool_ (Js_json.reifyType json) record_name field_name 

let string_ : 
  type a. (a Js_json.kind * a) -> string -> string -> string
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.String -> v 
  | Js_json.Null -> ""
  | _ -> E.unexpected_json_type record_name field_name

let string json record_name field_name = 
  string_ (Js_json.reifyType json) record_name field_name

let bytes _ record_name field_name = 
  E.unexpected_json_type record_name field_name

let object__: 
  type a. (a Js_json.kind * a) -> string -> string -> Js_json.t Js_dict.t
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.Object -> v 
  | Js_json.Null -> Js_dict.empty () 
  | _ -> E.unexpected_json_type record_name field_name

let object_ json record_name field_name = 
  object__ (Js_json.reifyType json) record_name field_name

let array__: 
  type a. (a Js_json.kind * a) -> string -> string -> Js_json.t array
  = fun (ty, v) record_name field_name -> 

  match ty with
  | Js_json.Array -> v 
  | Js_json.Null -> [||]
  | _ -> E.unexpected_json_type record_name field_name
  
let array_ json record_name field_name = 
  array__ (Js_json.reifyType json) record_name field_name

