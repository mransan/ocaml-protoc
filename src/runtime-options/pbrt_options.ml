module E = struct
  type error =
    | Unexpected_option_type of string * string
    | Malformed_variant of string

  exception Failure of error

  let unexpected_option_type record_name field_name =
    raise (Failure (Unexpected_option_type (record_name, field_name)))

  let malformed_variant variant_name =
    raise (Failure (Malformed_variant variant_name))

  let string_of_error = function
    | Unexpected_option_type (record_name, field_name) ->
      Printf.sprintf "Unexpected option type (record name:%s, field_name:%s)"
        record_name field_name
    | Malformed_variant variant_name ->
      Printf.sprintf "Malformed variant (variant name: %s)" variant_name

  let () =
    Printexc.register_printer (fun exn ->
        match exn with
        | Failure e -> Some (string_of_error e)
        | _ -> None)
end

type constant =
  | Constant_string of string
  | Constant_bool of bool
  | Constant_int of int
  | Constant_float of float
  | Constant_literal of string

type message_literal = (string * value) list
and list_literal = value list

and value =
  | Scalar_value of constant
  | Message_literal of message_literal
  | List_literal of list_literal

let unescape_string str =
  let buffer = Buffer.create (String.length str) in
  let rec aux i =
    if i < String.length str then (
      match str.[i] with
      | '\\' ->
        (match str.[i + 1] with
        | 'a' ->
          Buffer.add_char buffer '\007';
          aux (i + 2)
        | 'b' ->
          Buffer.add_char buffer '\b';
          aux (i + 2)
        | 'f' ->
          Buffer.add_char buffer '\012';
          aux (i + 2)
        | 'n' ->
          Buffer.add_char buffer '\n';
          aux (i + 2)
        | 'r' ->
          Buffer.add_char buffer '\r';
          aux (i + 2)
        | 't' ->
          Buffer.add_char buffer '\t';
          aux (i + 2)
        | 'v' ->
          Buffer.add_char buffer '\011';
          aux (i + 2)
        | '?' ->
          Buffer.add_char buffer '?';
          aux (i + 2)
        | '\\' ->
          Buffer.add_char buffer '\\';
          aux (i + 2)
        | '\'' ->
          Buffer.add_char buffer '\'';
          aux (i + 2)
        | '"' ->
          Buffer.add_char buffer '"';
          aux (i + 2)
        | 'x' ->
          (* handle hexadecimal escape *)
          let hex = String.sub str (i + 2) 2 in
          Buffer.add_char buffer (Char.chr (int_of_string ("0x" ^ hex)));
          aux (i + 4)
        | 'u' ->
          (* handle Unicode escape with 4 hex digits *)
          let unicode = String.sub str (i + 2) 4 in
          Buffer.add_char buffer (Char.chr (int_of_string ("0x" ^ unicode)));
          aux (i + 6)
        | 'U' ->
          (* handle Unicode escape with 5 hex digits *)
          let unicode = String.sub str (i + 2) 5 in
          Buffer.add_char buffer (Char.chr (int_of_string ("0x" ^ unicode)));
          aux (i + 7)
        | c when c >= '0' && c <= '7' ->
          (* handle octal escape *)
          let end_idx = min (i + 4) (String.length str) in
          let rec find_octal_end idx =
            if idx < end_idx && str.[idx] >= '0' && str.[idx] <= '7' then
              find_octal_end (idx + 1)
            else
              idx
          in
          let octal_end = find_octal_end (i + 2) in
          let octal = String.sub str (i + 1) (octal_end - i - 1) in
          Buffer.add_char buffer (Char.chr (int_of_string ("0o" ^ octal)));
          aux octal_end
        | c -> failwith (Printf.sprintf "Invalid escape sequence: \\%c" c))
      | c ->
        Buffer.add_char buffer c;
        aux (i + 1)
    )
  in
  aux 0;
  Buffer.contents buffer

let int32 v record_name field_name =
  match v with
  | Scalar_value (Constant_float v) -> Int32.of_float v
  | Scalar_value (Constant_int v) -> Int32.of_int v
  | _ -> E.unexpected_option_type record_name field_name

let float v record_name field_name =
  match v with
  | Scalar_value (Constant_float v) -> v
  | Scalar_value (Constant_int v) -> float_of_int v
  | _ -> E.unexpected_option_type record_name field_name

let int64 v record_name field_name =
  match v with
  | Scalar_value (Constant_float v) -> Int64.of_float v
  | Scalar_value (Constant_int v) -> Int64.of_int v
  | _ -> E.unexpected_option_type record_name field_name

let int v record_name field_name =
  match v with
  | Scalar_value (Constant_float v) -> int_of_float v
  | Scalar_value (Constant_int v) -> v
  | _ -> E.unexpected_option_type record_name field_name

let string v record_name field_name =
  match v with
  | Scalar_value (Constant_string v) -> unescape_string v
  | _ -> E.unexpected_option_type record_name field_name

let bool v record_name field_name =
  match v with
  | Scalar_value (Constant_bool v) -> v
  | _ -> E.unexpected_option_type record_name field_name

let bytes v record_name field_name =
  string v record_name field_name |> Bytes.of_string

let unit v record_name field_name =
  match v with
  | Message_literal [] -> ()
  | _ -> E.unexpected_option_type record_name field_name
