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

module E = Pb_exception
module Pt = Pb_parsing_parse_tree
module Tt = Pb_typing_type_tree
module Typing_util = Pb_typing_util

(** Re-construct nested messages out of field path *)
let normalize_option_name option_name value =
  List.fold_right
    (fun name_part acc ->
      match name_part with
      | Pb_raw_option.Simple_name name ->
        Pb_option.Message_literal [ name, acc ]
      | Pb_raw_option.Extension_name name ->
        (* TODO: Consider supporting Extension_name in option names, as in
           (foo).bar.(.baz.bob) *)
        failwith
          (Printf.sprintf
             "normalize_option_name: Extension_name '%s' is not supported in \
              option_name"
             name))
    option_name value

let option_name_from_part = function
  | Pb_raw_option.Simple_name x -> Pb_option.Simple_name x
  | Pb_raw_option.Extension_name x -> Pb_option.Extension_name x

let normalize_option option_name value =
  match option_name with
  | [] -> failwith "option_name can't be an empty list!"
  | [ single_item ] ->
    (* Only one top level component in option name - nothing fancy is required
       in this case *)
    option_name_from_part single_item, value
  | top_level_item :: rest ->
    (* Top level option is a message, and we need to reconstruct nested messages
       inside the value, and leave only top level name component as actual option
       name *)
    let new_value = normalize_option_name rest value in
    option_name_from_part top_level_item, new_value

(** [compile_options set] is compiling raw options into Pb_option.set *)
let compile_options option_set =
  (* Handle destructured lists *)
  let option_set = Pb_raw_option.group_list_values option_set in
  List.fold_left
    (fun set (option_name, value) ->
      (* Normalize option names into nested messages within values *)
      let option_name, value = normalize_option option_name value in
      (* Pb_option.add is smart to merge nested messages, reconstructing the
         final message values *)
      Pb_option.add set option_name value)
    Pb_option.empty option_set

let scope_of_package : string option -> Tt.type_scope = function
  | Some s ->
    {
      Typing_util.empty_scope with
      Tt.packages = List.rev @@ Pb_util.rev_split_by_char '.' s;
    }
  | None -> Typing_util.empty_scope

let get_default field_name field_options : Pb_option.constant option =
  match Pb_raw_option.get_simple field_options "default" with
  | Some (Pb_option.Scalar_value constant) -> Some constant
  | Some (Pb_option.Message_literal _) ->
    E.invalid_default_value ~field_name
      ~info:"message literals are unsupported for default values" ()
  | Some (Pb_option.List_literal _) ->
    E.invalid_default_value ~field_name
      ~info:"list literals are unsupported for default values" ()
  | None -> None

let compile_field_p1 field_parsed : _ Tt.field =
  let { Pt.field_type; Pt.field_options; Pt.field_name; _ } = field_parsed in

  let field_default = get_default field_name field_options in
  let field_options = compile_options field_options in
  { Tt.field_parsed; Tt.field_type; Tt.field_default; Tt.field_options }

let compile_map_p1 map_parsed : _ Tt.map_field =
  let {
    Pt.map_name;
    Pt.map_number;
    Pt.map_key_type;
    Pt.map_value_type;
    Pt.map_options;
  } =
    map_parsed
  in
  let map_options = compile_options map_options in
  Tt.{ map_name; map_number; map_key_type; map_value_type; map_options }

let compile_oneof_p1 oneof_parsed : _ Tt.oneof =
  let init =
    {
      Tt.oneof_name = oneof_parsed.Pt.oneof_name;
      Tt.oneof_fields = [];
      Tt.oneof_options = Pb_option.empty;
    }
  in
  (* Compile one-of options separately, as compilation requires to see all raw
     options at once *)
  let oneof_options =
    oneof_parsed.Pt.oneof_body
    |> Pb_util.List.filter_map (function
         | Pt.Oneof_option o -> Some o
         | _ -> None)
    |> List.fold_left
         (fun oneof_options (name, value) ->
           Pb_raw_option.add oneof_options name value)
         Pb_raw_option.empty
    |> compile_options
  in
  let oneof =
    List.fold_left
      (fun acc -> function
        | Pt.Oneof_field f ->
          {
            acc with
            Tt.oneof_fields = compile_field_p1 f :: acc.Tt.oneof_fields;
          }
        | _ -> acc)
      init oneof_parsed.Pt.oneof_body
  in
  (* now reverse the fields so they're back in the original order *)
  { oneof with Tt.oneof_fields = List.rev oneof.oneof_fields; Tt.oneof_options }

let not_found f : bool =
  try
    f ();
    false
  with Not_found -> true

let rec list_assoc2 x = function
  | [] -> raise Not_found
  | (a, b) :: l ->
    if compare b x = 0 then
      a
    else
      list_assoc2 x l

let make_proto_type ~file_name ~file_options ~id ~scope ~spec : _ Tt.proto_type
    =
  { Tt.id; Tt.scope; Tt.file_name; Tt.file_options; Tt.spec }

(** compile a [Pbpt] enum to a [Pbtt] type *)
let compile_enum_p1 file_name file_options scope parsed_enum =
  let { Pt.enum_id; enum_name; enum_body } = parsed_enum in

  let enum_values =
    Pb_util.List.filter_map
      (function
        | Pt.Enum_value
            { Pt.enum_value_name; enum_value_int; enum_value_options } ->
          let enum_value_options = compile_options enum_value_options in
          Some Tt.{ enum_value_name; enum_value_int; enum_value_options }
        | _ -> None)
      enum_body
  in

  (* Compile enum options separately, as compilation requires to see all raw
     options at once *)
  let enum_options =
    enum_body
    |> Pb_util.List.filter_map (function
         | Pt.Enum_option o -> Some o
         | _ -> None)
    |> List.fold_left
         (fun enum_options (name, value) ->
           Pb_raw_option.add enum_options name value)
         Pb_raw_option.empty
    |> compile_options
  in

  let spec = Tt.Enum { Tt.enum_name; Tt.enum_values; Tt.enum_options } in

  make_proto_type ~file_name ~file_options ~id:enum_id ~scope ~spec

(** compile a [Pbpt] message a list of [Pbtt] types (ie messages can
    defined more than one type). *)
let rec validate_message file_name file_options message_scope parsed_message :
    _ Tt.proto_type list =
  let { Pt.id; Pt.message_name; Pt.message_body } = parsed_message in

  let { Tt.message_names; _ } = message_scope in
  let sub_scope =
    { message_scope with Tt.message_names = message_names @ [ message_name ] }
  in

  let module Acc = struct
    (* Ad-hoc module for the "large" accumulated data during the
       fold_left below.
    *)

    type ('a, 'b, 'd) t = {
      message_body: 'a list;
      extensions: 'b list;
      options: Pb_raw_option.set;
      all_types: 'd list;
    }

    let e0 =
      { message_body = []; extensions = []; options = []; all_types = [] }
  end in
  let acc =
    List.fold_left
      (fun acc field ->
        let { Acc.message_body; extensions; options; all_types } = acc in

        match field with
        | Pt.Message_field f ->
          let field = Tt.Message_field (compile_field_p1 f) in
          { acc with Acc.message_body = field :: message_body }
        | Pt.Message_map_field m ->
          let field = Tt.Message_map_field (compile_map_p1 m) in
          { acc with Acc.message_body = field :: message_body }
        | Pt.Message_oneof_field o ->
          let field = Tt.Message_oneof_field (compile_oneof_p1 o) in
          { acc with Acc.message_body = field :: message_body }
        | Pt.Message_sub m ->
          let all_sub_types =
            validate_message file_name file_options sub_scope m
          in
          { acc with Acc.all_types = all_types @ all_sub_types }
        | Pt.Message_enum parsed_enum ->
          let enum =
            compile_enum_p1 file_name file_options sub_scope parsed_enum
          in
          { acc with Acc.all_types = all_types @ [ enum ] }
        | Pt.Message_extension extension_ranges ->
          { acc with Acc.extensions = extensions @ extension_ranges }
        | Pt.Message_reserved _ ->
          acc (* TODO add support for checking reserved fields *)
        | Pt.Message_option (name, value) ->
          let options = Pb_raw_option.add options name value in
          { acc with Acc.options })
      Acc.e0 message_body
  in

  let message_body = List.rev acc.Acc.message_body in

  (* TODO: Maybe [validate_duplicate] should be in
     [Pb_parsing_util.verify_message] along with
     the proto3 invariant. *)

  (* Both field name and field number must be unique
     within a message scope. This includes the field in a
     oneof field inside the message.

     This function verifies this constrain and raises
     the corresponding Duplicated_field_number exception in
     case it is violated. *)
  let validate_duplicate (number_index : (int * string) list) name number =
    if
      not_found (fun () -> ignore @@ List.assoc number number_index)
      && not_found (fun () -> ignore @@ list_assoc2 name number_index)
    then
      (number, name) :: number_index
    else
      E.duplicated_field_number ~field_name:name ~previous_field_name:""
        ~message_name ()
  in

  let validate_duplicate_field number_index field =
    let number = Typing_util.field_number field in
    let name = Typing_util.field_name field in
    validate_duplicate number_index name number
  in

  ignore
    (List.fold_left
       (fun number_index -> function
         | Tt.Message_field field -> validate_duplicate_field number_index field
         | Tt.Message_oneof_field { Tt.oneof_fields; _ } ->
           List.fold_left validate_duplicate_field number_index oneof_fields
         | Tt.Message_map_field m ->
           let { Tt.map_name; map_number; _ } = m in
           validate_duplicate number_index map_name map_number)
       [] message_body
      : _ list);

  let spec =
    Tt.Message
      {
        Tt.extensions = acc.Acc.extensions;
        message_options = acc.Acc.options |> compile_options;
        message_name;
        message_body;
      }
  in

  acc.Acc.all_types
  @ [ make_proto_type ~file_name ~file_options ~id ~scope:message_scope ~spec ]

let validate_service (scope : Tt.type_scope) ~file_name (service : Pt.service) :
    _ Tt.service =
  let { Pt.service_name; service_body } = service in
  let service_body =
    List.filter_map
      (function
        | Pt.Service_option _ -> None
        | Pt.Service_rpc
            {
              rpc_name;
              rpc_options;
              rpc_req_stream;
              rpc_req;
              rpc_res_stream;
              rpc_res;
            } ->
          let rpc_req =
            match rpc_req with
            | `User_defined ty -> ty
            | _ -> E.invalid_rpc_req_type ~service_name ~rpc_name ()
          in
          let rpc_res =
            match rpc_res with
            | `User_defined ty -> ty
            | _ -> E.invalid_rpc_res_type ~service_name ~rpc_name ()
          in
          let rpc_options = compile_options rpc_options in
          let rpc =
            {
              Tt.rpc_name;
              rpc_options;
              rpc_req_stream;
              rpc_req;
              rpc_res_stream;
              rpc_res;
            }
          in
          Some rpc)
      service_body
  in
  {
    Tt.service_packages = scope.packages;
    service_file_name = file_name;
    service_name;
    service_body;
  }

let validate (proto : Pt.proto) : _ Tt.proto =
  let {
    Pt.package;
    Pt.proto_file_name;
    messages;
    enums;
    file_options;
    services;
    _;
  } =
    proto
  in

  let file_name = Pb_util.Option.default "" proto_file_name in
  let scope = scope_of_package package in
  let file_options = compile_options file_options in

  let pbtt_msgs =
    List.fold_right
      (fun e pbtt_msgs ->
        [ compile_enum_p1 file_name file_options scope e ] :: pbtt_msgs)
      enums []
  in

  let proto_types =
    List.fold_left
      (fun pbtt_msgs pbpt_msg ->
        let tys = validate_message file_name file_options scope pbpt_msg in
        tys :: pbtt_msgs)
      pbtt_msgs messages
  in

  let proto_services = List.map (validate_service scope ~file_name) services in
  { Tt.proto_types; proto_services }
