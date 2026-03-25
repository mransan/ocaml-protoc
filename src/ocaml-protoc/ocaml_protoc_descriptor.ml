module Tt = Pb_typing_type_tree
module Pt = Pb_parsing_parse_tree
module Tu = Pb_typing_util
module D = Descriptor

(** Fully-qualified proto name for a resolved type ID, with leading dot. *)
let fq_name all_types id =
  let t = Tu.type_of_id all_types id in
  let name =
    match t.Tt.spec with
    | Tt.Message { Tt.message_name; _ } -> message_name
    | Tt.Enum { Tt.enum_name; _ } -> enum_name
  in
  "."
  ^ String.concat "."
      (t.Tt.scope.Tt.packages @ t.Tt.scope.Tt.message_names @ [ name ])

let field_type_of_builtin :
    Pb_field_type.builtin_type -> D.field_descriptor_proto_type = function
  | `Double -> D.Type_double
  | `Float -> D.Type_float
  | `Int32 -> D.Type_int32
  | `Int64 -> D.Type_int64
  | `Uint32 -> D.Type_uint32
  | `Uint64 -> D.Type_uint64
  | `Sint32 -> D.Type_sint32
  | `Sint64 -> D.Type_sint64
  | `Fixed32 -> D.Type_fixed32
  | `Fixed64 -> D.Type_fixed64
  | `Sfixed32 -> D.Type_sfixed32
  | `Sfixed64 -> D.Type_sfixed64
  | `Bool -> D.Type_bool
  | `String -> D.Type_string
  | `Bytes -> D.Type_bytes

let label_of_pt : Pt.message_field_label -> D.field_descriptor_proto_label =
  function
  | `Optional | `Nolabel -> D.Label_optional
  | `Required -> D.Label_required
  | `Repeated -> D.Label_repeated

(** Resolve a field_type to (type_, type_name option). *)
let resolve_field_type ~all_types (ft : Pb_field_type.resolved Pb_field_type.t)
    =
  match ft with
  | #Pb_field_type.builtin_type as bt -> field_type_of_builtin bt, None
  | `User_defined id ->
    let type_ =
      match (Tu.type_of_id all_types id).Tt.spec with
      | Tt.Message _ -> D.Type_message
      | Tt.Enum _ -> D.Type_enum
    in
    type_, Some (fq_name all_types id)

let field_of_message_field ~all_types ~oneof_index
    (f : Pb_field_type.resolved Tt.message_field) =
  let type_, type_name = resolve_field_type ~all_types f.Tt.field_type in
  D.make_field_descriptor_proto ~name:f.Tt.field_parsed.Pt.field_name
    ~number:(Int32.of_int f.Tt.field_parsed.Pt.field_number)
    ~label:(label_of_pt f.Tt.field_parsed.Pt.field_label)
    ~type_ ?type_name ~json_name:f.Tt.field_parsed.Pt.field_name
    ?oneof_index:(Option.map Int32.of_int oneof_index)
    ()

let field_of_oneof_field ~all_types ~oneof_index
    (f : Pb_field_type.resolved Tt.oneof_field) =
  let type_, type_name = resolve_field_type ~all_types f.Tt.field_type in
  D.make_field_descriptor_proto ~name:f.Tt.field_parsed.Pt.field_name
    ~number:(Int32.of_int f.Tt.field_parsed.Pt.field_number)
    ~label:D.Label_optional ~type_ ?type_name
    ~json_name:f.Tt.field_parsed.Pt.field_name
    ~oneof_index:(Int32.of_int oneof_index) ()

(** PascalCase from snake_case, for MapEntry type names. *)
let pascal_case s =
  String.concat ""
    (List.map String.capitalize_ascii (String.split_on_char '_' s))

let message_descriptor ~all_types ~scope
    (msg : Pb_field_type.resolved Tt.message) =
  let fields = ref [] in
  let nested = ref [] in
  let oneofs = ref [] in
  let next_oneof_index = ref 0 in
  List.iter
    (fun body_item ->
      match body_item with
      | Tt.Message_field f ->
        fields :=
          !fields @ [ field_of_message_field ~all_types ~oneof_index:None f ]
      | Tt.Message_oneof_field oneof ->
        oneofs :=
          !oneofs
          @ [ D.make_oneof_descriptor_proto ~name:oneof.Tt.oneof_name () ];
        let idx = !next_oneof_index in
        incr next_oneof_index;
        List.iter
          (fun f ->
            fields :=
              !fields @ [ field_of_oneof_field ~all_types ~oneof_index:idx f ])
          oneof.Tt.oneof_fields
      | Tt.Message_map_field map ->
        let entry_name = pascal_case map.Tt.map_name ^ "Entry" in
        let key_type =
          field_type_of_builtin
            (map.Tt.map_key_type :> Pb_field_type.builtin_type)
        in
        let val_type, val_type_name =
          resolve_field_type ~all_types map.Tt.map_value_type
        in
        let entry =
          D.make_descriptor_proto ~name:entry_name
            ~field:
              [
                D.make_field_descriptor_proto ~name:"key" ~number:1l
                  ~label:D.Label_optional ~type_:key_type ();
                D.make_field_descriptor_proto ~name:"value" ~number:2l
                  ~label:D.Label_optional ~type_:val_type
                  ?type_name:val_type_name ();
              ]
            ~options:(D.make_message_options ~map_entry:true ())
            ()
        in
        nested := !nested @ [ entry ];
        let entry_fq =
          "."
          ^ String.concat "."
              (scope.Tt.packages @ scope.Tt.message_names
              @ [ msg.Tt.message_name; entry_name ])
        in
        fields :=
          !fields
          @ [
              D.make_field_descriptor_proto ~name:map.Tt.map_name
                ~number:(Int32.of_int map.Tt.map_number)
                ~label:D.Label_repeated ~type_:D.Type_message
                ~type_name:entry_fq ();
            ])
    msg.Tt.message_body;
  D.make_descriptor_proto ~name:msg.Tt.message_name ~field:!fields
    ~nested_type:!nested ~oneof_decl:!oneofs ()

let enum_descriptor (e : Tt.enum) =
  D.make_enum_descriptor_proto ~name:e.Tt.enum_name
    ~value:
      (List.map
         (fun ev ->
           D.make_enum_value_descriptor_proto ~name:ev.Tt.enum_value_name
             ~number:(Int32.of_int ev.Tt.enum_value_int)
             ())
         e.Tt.enum_values)
    ()

let service_descriptor ~all_types (svc : Pb_field_type.resolved Tt.service) =
  D.make_service_descriptor_proto ~name:svc.Tt.service_name
    ~method_:
      (List.map
         (fun (rpc : Pb_field_type.resolved Tt.rpc) ->
           D.make_method_descriptor_proto ~name:rpc.Tt.rpc_name
             ~input_type:(fq_name all_types rpc.Tt.rpc_req)
             ~output_type:(fq_name all_types rpc.Tt.rpc_res)
             ?client_streaming:
               (if rpc.Tt.rpc_req_stream then
                  Some true
                else
                  None)
             ?server_streaming:
               (if rpc.Tt.rpc_res_stream then
                  Some true
                else
                  None)
             ())
         svc.Tt.service_body)
    ()

let to_file_descriptor_proto
    ~(all_types : Pb_field_type.resolved Tt.proto_type list) ~proto_file_name
    ~(typed_proto : Pb_field_type.resolved Tt.proto) =
  let flat_types = List.flatten typed_proto.Tt.proto_types in
  let package =
    match flat_types with
    | { Tt.scope = { Tt.packages; _ }; _ } :: _ -> String.concat "." packages
    | [] ->
      (match typed_proto.Tt.proto_services with
      | { Tt.service_packages; _ } :: _ -> String.concat "." service_packages
      | [] -> "")
  in
  let message_type, enum_type =
    List.fold_right
      (fun pt (msgs, enums) ->
        match pt.Tt.spec with
        | Tt.Message msg ->
          message_descriptor ~all_types ~scope:pt.Tt.scope msg :: msgs, enums
        | Tt.Enum e -> msgs, enum_descriptor e :: enums)
      flat_types ([], [])
  in
  D.make_file_descriptor_proto
    ~name:(Filename.basename proto_file_name)
    ?package:
      (if package = "" then
         None
       else
         Some package)
    ~syntax:"proto3" ~message_type ~enum_type
    ~service:
      (List.map (service_descriptor ~all_types) typed_proto.Tt.proto_services)
    ()

let write_json ~out_file
    ~(all_types : Pb_field_type.resolved Tt.proto_type list) ~proto_file_name
    ~(typed_proto : Pb_field_type.resolved Tt.proto) =
  let file_desc =
    to_file_descriptor_proto ~all_types ~proto_file_name ~typed_proto
  in
  let set = D.make_file_descriptor_set ~file:[ file_desc ] () in
  let json = D.encode_json_file_descriptor_set set in
  let oc = open_out out_file in
  try
    output_string oc (Yojson.Basic.pretty_to_string json);
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e
