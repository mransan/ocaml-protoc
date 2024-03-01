type name_part = Pb_option.option_name =
  | Simple_name of string
  | Extension_name of string

type option_name = name_part list
type t = option_name * Pb_option.value
type set = t list

let stringify_option_name name =
  name
  |> List.map (function
       | Simple_name s -> s
       | Extension_name s -> "(" ^ s ^ ")")
  |> String.concat "."

let option_name_part_equal a b =
  match a, b with
  | Simple_name a, Simple_name b -> String.equal a b
  | Extension_name a, Extension_name b -> String.equal a b
  | _ -> false

let option_name_equal = List.equal option_name_part_equal
let empty = []
let add option_set option_name value = (option_name, value) :: option_set

let merge set1 set2 =
  List.fold_left
    (fun acc (option_name, value) -> add acc option_name value)
    set1 set2

let get t option_name =
  match List.find (fun (other, _) -> option_name_equal option_name other) t with
  | _, c -> Some c
  | exception Not_found -> None

let get_ext t option_name = get t [ Extension_name option_name ]
let get_simple t option_name = get t [ Simple_name option_name ]

let pp_t ppf (name, value) =
  Format.fprintf ppf "{@;<1 2>%S: %a@;<1 2>}"
    (stringify_option_name name)
    Pb_option.pp_value value

let pp_set ppf set =
  Format.fprintf ppf "[@[<v>%a@]]"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp_t)
    set
