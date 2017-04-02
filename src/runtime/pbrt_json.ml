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

let int32 v record_name field_name = 
  match v with
  | `String v -> Int32.of_string v 
  | `Float f -> Int32.of_float f 
  | `Int i -> Int32.of_int i 
  | `Null -> 0l 
  | _ -> E.unexpected_json_type record_name field_name  

let float v record_name field_name = 
  match v with
  | `String v -> float_of_string v 
  | `Float f -> f 
  | `Int i -> float_of_int i 
  | `Null -> 0.0 
  | _ -> E.unexpected_json_type record_name field_name  

let int64 v record_name field_name = 
  match v with
  | `String v -> Int64.of_string v 
  | `Float f -> Int64.of_float f 
  | `Int i -> Int64.of_int i 
  | `Null -> 0L 
  | _ -> E.unexpected_json_type record_name field_name  

let int v record_name field_name = 
  match v with
  | `String v -> int_of_string v 
  | `Float f -> int_of_float f 
  | `Int i -> i 
  | `Null -> 0 
  | _ -> E.unexpected_json_type record_name field_name  

let string v record_name field_name = 
  match v with
  | `String v -> v 
  | `Null -> ""
  | _ -> E.unexpected_json_type record_name field_name

let bool v record_name field_name = 
  match v with
  | `Bool b -> b 
  | `Null -> false
  | _ -> E.unexpected_json_type record_name field_name 

let bytes _ record_name field_name = 
  E.unexpected_json_type record_name field_name

let make_bool v = `Bool  v
let make_int v = `Int  v
let make_float v = `Float  v
let make_string v = `String  v
let make_list v = `List v
