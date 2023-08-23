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

(** Protobuf parse tree *)

type message_field_label =
  [ `Optional
  | `Required
  | `Repeated
  | `Nolabel (* proto3 field which replaces required and optional *)
  ]
(** A field property defining its occurence
*)

type oneof_field_label = unit
(** Oneof field fields label 

    Oneof fields have no label, they are simply choices for the 
    oneof fiel they belong to. *)

type 'a field = {
  field_name: string;
  field_number: int;
  field_label: 'a;
  field_type: Pb_field_type.unresolved_t;
  field_options: Pb_option.set;
}
(** message field. 

    Note this field is parametrized with the label type 
    so that it can be used both by normal field and one of 
    field since the only difference between the 2 is 
    the label.
*)

type message_field = message_field_label field
type oneof_field = oneof_field_label field

type map_field = {
  map_name: string;
  map_number: int;
  map_key_type: Pb_field_type.map_key_type;
  map_value_type: Pb_field_type.unresolved_t;
  map_options: Pb_option.set;
}

type oneof_body_content =
  | Oneof_field of oneof_field
  | Oneof_option of Pb_option.t

type oneof = {
  oneof_name: string;
  oneof_body: oneof_body_content list;
}
(** oneof entity *)

type enum_value = {
  enum_value_name: string;
  enum_value_int: int;
}

type enum_body_content =
  | Enum_value of enum_value
  | Enum_option of Pb_option.t

type enum = {
  enum_id: int;
  enum_name: string;
  enum_body: enum_body_content list;
}

type extension_range_to =
  | To_max
  | To_number of int

type extension_range_from = int

type extension_range =
  | Extension_single_number of int
  | Extension_range of extension_range_from * extension_range_to

(** Body content defines all the possible consituant 
    of a message. 
*)
type message_body_content =
  | Message_field of message_field
  | Message_map_field of map_field
  | Message_oneof_field of oneof
  | Message_sub of message
  | Message_enum of enum
  | Message_extension of extension_range list
  | Message_reserved of extension_range list
  | Message_option of Pb_option.t

and message = {
  id: int;
  message_name: string;
  message_body: message_body_content list;
}
(** Message entity. 

    Note the ID is simply for uniquely (and easily) identifying a type. It is
    expected to be generated by a parser. The later compilation 
    functions expects this id to be unique.
*)

type rpc = {
  rpc_name: string;
  rpc_options: Pb_option.set;
  rpc_req_stream: bool;
  rpc_req: Pb_field_type.unresolved_t;
  rpc_res_stream: bool;
  rpc_res: Pb_field_type.unresolved_t;
}

type service_body_content =
  | Service_rpc of rpc
  | Service_option of Pb_option.t

type service = {
  service_name: string;
  service_body: service_body_content list;
}

type extend = {
  id: int;
  extend_name: string;
  extend_body: message_field list;
}

type import = {
  file_name: string;
  public: bool;
}

type proto = {
  proto_file_name: string option;
  syntax: string option;
  imports: import list;
  file_options: Pb_option.set;
  package: string option;
  messages: message list;
  services: service list;
  enums: enum list;
  extends: extend list;
}
(** Definition of a protobuffer message file. 
*)

[@@@warning "-44"]

open Format
open Pb_format_util

let pp_message_field_label ppf label =
  let label_str =
    match label with
    | `Optional -> "Optional"
    | `Required -> "Required"
    | `Repeated -> "Repeated"
    | `Nolabel -> "Nolabel"
  in
  fprintf ppf "`%s" label_str

let pp_oneof_field_label _ppf () = ()

let pp_field pp_label ppf field =
  fprintf ppf
    "{@[<v 2>@,\
     field_name = %S;@,\
     field_number = %d;@,\
     field_label = %a;@,\
     field_type = %a;@,\
     field_options = %a;@,\
     }@]"
    field.field_name field.field_number pp_label field.field_label
    Pb_field_type.pp_unresolved_t field.field_type Pb_option.pp_set
    field.field_options

let pp_message_field ppf field = pp_field pp_message_field_label ppf field
let pp_oneof_field ppf field = pp_field pp_oneof_field_label ppf field

let pp_map_field ppf map_field =
  fprintf ppf
    "{@[<v 2>@,\
     map_name = %S;@,\
     map_number = %d;@,\
     map_key_type = %a;@,\
     map_value_type = %a;@,\
     map_options = %a;@,\
     }@]"
    map_field.map_name map_field.map_number Pb_field_type.pp_map_key_type
    map_field.map_key_type Pb_field_type.pp_unresolved_t
    map_field.map_value_type Pb_option.pp_set map_field.map_options

let pp_oneof_body_content ppf = function
  | Oneof_field field -> pp_oneof_field ppf field
  | Oneof_option option -> Pb_option.pp_t ppf option

let pp_oneof ppf oneof =
  fprintf ppf "{@[<v 2>%s = %S;@,%s = [@[<v>%a@]];@,@]}" "oneof_name"
    oneof.oneof_name "oneof_body"
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf ";@,")
       pp_oneof_body_content)
    oneof.oneof_body

let pp_enum_value ppf enum_value =
  fprintf ppf "{@[<v 2>@,enum_value_name = %S;@,enum_value_int = %d;@,}@]"
    enum_value.enum_value_name enum_value.enum_value_int

let pp_enum_body_content ppf enum_body_content =
  match enum_body_content with
  | Enum_value enum_value -> pp_enum_value ppf enum_value
  | Enum_option option -> Pb_option.pp_t ppf option

let pp_enum ppf enum =
  fprintf ppf
    "{@[<v 2>@,enum_id = %d;@,enum_name = %S;@,enum_body = [@[<v>%a@]];@,}@]"
    enum.enum_id enum.enum_name
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf ";@,")
       pp_enum_body_content)
    enum.enum_body

let pp_extension_range_to ppf ext_range_to =
  match ext_range_to with
  | To_max -> fprintf ppf "To_max"
  | To_number n -> fprintf ppf "(To_number %d)" n

let pp_extension_range_from ppf ext_range_from = fprintf ppf "%d" ext_range_from

let pp_extension_range ppf ext_range =
  match ext_range with
  | Extension_single_number n -> fprintf ppf "(Extension_single_number %d)" n
  | Extension_range (from, to_) ->
    fprintf ppf "(Extension_range (%d, %a))" from pp_extension_range_to to_

let rec pp_message_body_content ppf msg_body_content =
  match msg_body_content with
  | Message_field field -> pp_message_field ppf field
  | Message_map_field map_field -> pp_map_field ppf map_field
  | Message_oneof_field oneof_field -> pp_oneof ppf oneof_field
  | Message_sub sub_message -> pp_message ppf sub_message
  | Message_enum enum -> pp_enum ppf enum
  | Message_extension ext_ranges ->
    fprintf ppf "Message_extension [@[<v>%a@]]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ";@,")
         pp_extension_range)
      ext_ranges
  | Message_reserved res_ranges ->
    fprintf ppf "Message_reserved [@[<v>%a@]]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ";@,")
         pp_extension_range)
      res_ranges
  | Message_option option -> Pb_option.pp_t ppf option

and pp_message ppf message =
  fprintf ppf
    "{@[<v 2>@,id = %d;@,message_name = %S;@,message_body = [@[<v>%a@]];@,}@]"
    message.id message.message_name
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf ";@,")
       pp_message_body_content)
    message.message_body

let pp_rpc ppf rpc =
  fprintf ppf
    "{@[<v 2>@,\
     rpc_name = %S;@,\
     rpc_options = %a;@,\
     rpc_req_stream = %b;@,\
     rpc_req = %a;@,\
     rpc_res_stream = %b;@,\
     rpc_res = %a;@,\
     }@]"
    rpc.rpc_name Pb_option.pp_set rpc.rpc_options rpc.rpc_req_stream
    Pb_field_type.pp_unresolved_t rpc.rpc_req rpc.rpc_res_stream
    Pb_field_type.pp_unresolved_t rpc.rpc_res

let rec pp_service_body_content ppf service_body_content =
  match service_body_content with
  | Service_rpc rpc -> pp_rpc ppf rpc
  | Service_option option -> Pb_option.pp_t ppf option

and pp_service ppf service =
  fprintf ppf "{@[<v 2>@,service_name = %S;@,service_body = [@[<v>%a@]];@,}@]"
    service.service_name
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf ";@,")
       pp_service_body_content)
    service.service_body

let pp_extend ppf extend =
  fprintf ppf
    "{@[<v 2>@,id = %d;@,extend_name = %S;@,extend_body = [@[<v>%a@]];@,}@]"
    extend.id extend.extend_name
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@,") pp_message_field)
    extend.extend_body

let pp_import ppf import =
  fprintf ppf "{@[<2>@,file_name = %S;@,public = %b;@,}@]" import.file_name
    import.public

let pp_proto ppf proto =
  fprintf ppf
    "{@[<v 2>@ proto_file_name = %a;@,\
     syntax = %a;@,\
     imports = [@[<v>%a@]];@,\
     file_options = %a;@,\
     package = %a;@,\
     messages = [@[<v>%a@]];@,\
     services = [@[<v>%a@]];@,\
     enums = [@[<v>%a@]];@,\
     extends = [@[<v>%a@]];@,\
     }@]"
    (pp_print_option ~none:pp_none pp_print_string)
    proto.proto_file_name
    (pp_print_option ~none:pp_none pp_print_string)
    proto.syntax
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@,") pp_import)
    proto.imports Pb_option.pp_set proto.file_options
    (pp_print_option ~none:pp_none pp_print_string)
    proto.package
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@,") pp_message)
    proto.messages
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@,") pp_service)
    proto.services
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@,") pp_enum)
    proto.enums
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@,") pp_extend)
    proto.extends