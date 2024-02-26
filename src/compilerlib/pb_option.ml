type constant =
  | Constant_string of string
  | Constant_bool of bool
  | Constant_int of int
  | Constant_float of float
  | Constant_literal of string

type name_part =
  | Simple_name of string
  | Extension_name of string

type option_name = name_part list

type message_literal = (string * value) list
and list_literal = value list

and value =
  | Scalar_value of constant
  | Message_literal of message_literal
  | List_literal of list_literal

type t = option_name * value
type set = t list

let name_part_to_string = function
  | Simple_name s -> s
  | Extension_name s -> "(" ^ s ^ ")"

let stringify_option_name (option_name : option_name) : string =
  let str_list = List.map name_part_to_string option_name in
  String.concat "." str_list

let name_part_equal a b =
  match a, b with
  | Simple_name a, Simple_name b -> String.equal a b
  | Extension_name a, Extension_name b -> String.equal a b
  | _ -> false

let option_name_equal a b = List.equal name_part_equal a b
let empty = []
let add t option_name value = (option_name, value) :: t
let merge t1 t2 = t2 @ t1

let get t option_name =
  match List.find (fun (other, _) -> option_name_equal option_name other) t with
  | _, c -> Some c
  | exception Not_found -> None

let get_ext t option_name = get t [ Extension_name option_name ]

let pp_constant ppf = function
  | Constant_string s -> Format.fprintf ppf "%S" s
  | Constant_bool b -> Format.fprintf ppf "%B" b
  | Constant_int i -> Format.fprintf ppf "%d" i
  | Constant_float f -> Format.fprintf ppf "%f" f
  | Constant_literal l -> Format.fprintf ppf "`%s`" l

let rec pp_value ppf = function
  | Scalar_value c -> pp_constant ppf c
  | Message_literal ml -> pp_message_literal ppf ml
  | List_literal ml -> pp_list_literal ppf ml

and pp_message_literal ppf ml =
  Format.fprintf ppf "{@[<v>%a@]}"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,")
       pp_message_field)
    ml

and pp_list_literal ppf ml =
  Format.fprintf ppf "[@[<v>%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,")
       pp_value)
    ml

and pp_message_field ppf (field, value) =
  Format.fprintf ppf "%S: %a" field pp_value value

let pp_t ppf (name, value) =
  Format.fprintf ppf "{@;<1 2>%S: %a@;<1 2>}"
    (stringify_option_name name)
    pp_value value

let pp_set ppf set =
  Format.fprintf ppf "[@[<v>%a@]]"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp_t)
    set
