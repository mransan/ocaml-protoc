(*
  The MIT License (MIT)

  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)

type constant =
  | Constant_string of string
  | Constant_bool of bool
  | Constant_int of int
  | Constant_float of float
  | Constant_literal of string

type option_name =
  | Simple_name of string
  | Extension_name of string

type message_literal = (string * value) list
and list_literal = value list

and value =
  | Scalar_value of constant
  | Message_literal of message_literal
  | List_literal of list_literal

type t = option_name * value
type set = t list

let stringify_option_name = function
  | Simple_name s -> s
  | Extension_name s -> "(" ^ s ^ ")"

let option_name_equal a b =
  match a, b with
  | Simple_name a, Simple_name b -> String.equal a b
  | Extension_name a, Extension_name b -> String.equal a b
  | _ -> false

let empty = []

let rec merge_value v1 v2 =
  match v1, v2 with
  | Message_literal ml1, Message_literal ml2 ->
    let rec merge_lists list1 list2 =
      match list2 with
      | [] -> list1
      | (field, value) :: rest ->
        let updated_list, is_merged =
          List.fold_left
            (fun (acc, merged) (f, v) ->
              if f = field then (
                match value, v with
                | Message_literal _, Message_literal _ ->
                  acc @ [ f, merge_value value v ], true
                | _ -> acc @ [ f, value ], merged
              ) else
                acc @ [ f, v ], merged)
            ([], false) list1
        in
        if is_merged then
          merge_lists updated_list rest
        else
          merge_lists (updated_list @ [ field, value ]) rest
    in
    Message_literal (merge_lists ml1 ml2)
  | _ -> v2 (* FIXME: This overrides an existing value, which is not allowed *)

let add option_set option_name value =
  match
    List.partition
      (fun ((name, _) : t) -> option_name_equal name option_name)
      option_set
  with
  | [], _ -> (option_name, value) :: option_set
  | [ (_, existing_value) ], remainder ->
    let merged_value = merge_value existing_value value in
    (option_name, merged_value) :: remainder
  | _ ->
    failwith
      "This should not happen, partition should result in at most single item \
       in left component"

let get t option_name =
  match List.find (fun (other, _) -> option_name_equal option_name other) t with
  | _, c -> Some c
  | exception Not_found -> None

let get_ext t option_name = get t (Extension_name option_name)

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
