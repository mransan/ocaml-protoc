type constant =
  | Constant_string of string
  | Constant_bool of bool
  | Constant_int of int
  | Constant_float of float
  | Constant_literal of string

type option_name = string
type t = option_name * constant
type set = t list

let empty = []
let add t option_name constant = (option_name, constant) :: t
let merge t1 t2 = t2 @ t1

let get t option_name =
  match List.assoc option_name t with
  | c -> Some c
  | exception Not_found -> None

let pp_constant ppf = function
  | Constant_string s -> Format.fprintf ppf "%S" s
  | Constant_bool b -> Format.fprintf ppf "%B" b
  | Constant_int i -> Format.fprintf ppf "%d" i
  | Constant_float f -> Format.fprintf ppf "%f" f
  | Constant_literal l -> Format.fprintf ppf "`%s`" l

let pp_t ppf (name, const) =
  Format.fprintf ppf "{@;<1 2>%S: %a@;<1 2>}" name pp_constant const

let pp_set ppf set =
  Format.fprintf ppf "[@[<v>%a@]]"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp_t)
    set
