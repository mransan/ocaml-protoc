[@@@ocaml.warning "-23-27-30-39-44"]

type field_descriptor_proto_label =
  | Label_optional 
  | Label_required 
  | Label_repeated 

type field_descriptor_proto_type =
  | Type_double 
  | Type_float 
  | Type_int64 
  | Type_uint64 
  | Type_int32 
  | Type_fixed64 
  | Type_fixed32 
  | Type_bool 
  | Type_string 
  | Type_group 
  | Type_message 
  | Type_bytes 
  | Type_uint32 
  | Type_enum 
  | Type_sfixed32 
  | Type_sfixed64 
  | Type_sint32 
  | Type_sint64 

type field_options_ctype =
  | String 
  | Cord 
  | String_piece 

type field_options_jstype =
  | Js_normal 
  | Js_string 
  | Js_number 

type uninterpreted_option_name_part = {
  mutable name_part : string;
  mutable is_extension : bool;
}

type uninterpreted_option = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 6 fields *)
  mutable name : uninterpreted_option_name_part list;
  mutable identifier_value : string;
  mutable positive_int_value : int64;
  mutable negative_int_value : int64;
  mutable double_value : float;
  mutable string_value : bytes;
  mutable aggregate_value : string;
}

type field_options = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 7 fields *)
  mutable ctype : field_options_ctype;
  mutable packed : bool;
  mutable jstype : field_options_jstype;
  mutable lazy_ : bool;
  mutable unverified_lazy : bool;
  mutable deprecated : bool;
  mutable weak : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type field_descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 10 fields *)
  mutable name : string;
  mutable number : int32;
  mutable label : field_descriptor_proto_label;
  mutable type_ : field_descriptor_proto_type;
  mutable type_name : string;
  mutable extendee : string;
  mutable default_value : string;
  mutable oneof_index : int32;
  mutable json_name : string;
  mutable options : field_options option;
  mutable proto3_optional : bool;
}

type enum_value_options = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable deprecated : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type enum_value_descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable name : string;
  mutable number : int32;
  mutable options : enum_value_options option;
}

type enum_options = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable allow_alias : bool;
  mutable deprecated : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type enum_descriptor_proto_enum_reserved_range = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable start : int32;
  mutable end_ : int32;
}

type enum_descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable value : enum_value_descriptor_proto list;
  mutable options : enum_options option;
  mutable reserved_range : enum_descriptor_proto_enum_reserved_range list;
  mutable reserved_name : string list;
}

type extension_range_options = {
  mutable uninterpreted_option : uninterpreted_option list;
}

type descriptor_proto_extension_range = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable start : int32;
  mutable end_ : int32;
  mutable options : extension_range_options option;
}

type oneof_options = {
  mutable uninterpreted_option : uninterpreted_option list;
}

type oneof_descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable options : oneof_options option;
}

type message_options = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 4 fields *)
  mutable message_set_wire_format : bool;
  mutable no_standard_descriptor_accessor : bool;
  mutable deprecated : bool;
  mutable map_entry : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type descriptor_proto_reserved_range = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable start : int32;
  mutable end_ : int32;
}

type descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable field : field_descriptor_proto list;
  mutable extension : field_descriptor_proto list;
  mutable nested_type : descriptor_proto list;
  mutable enum_type : enum_descriptor_proto list;
  mutable extension_range : descriptor_proto_extension_range list;
  mutable oneof_decl : oneof_descriptor_proto list;
  mutable options : message_options option;
  mutable reserved_range : descriptor_proto_reserved_range list;
  mutable reserved_name : string list;
}

type method_options_idempotency_level =
  | Idempotency_unknown 
  | No_side_effects 
  | Idempotent 

type method_options = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable deprecated : bool;
  mutable idempotency_level : method_options_idempotency_level;
  mutable uninterpreted_option : uninterpreted_option list;
}

type method_descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable name : string;
  mutable input_type : string;
  mutable output_type : string;
  mutable options : method_options option;
  mutable client_streaming : bool;
  mutable server_streaming : bool;
}

type service_options = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable deprecated : bool;
  mutable uninterpreted_option : uninterpreted_option list;
}

type service_descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable name : string;
  mutable method_ : method_descriptor_proto list;
  mutable options : service_options option;
}

type file_options_optimize_mode =
  | Speed 
  | Code_size 
  | Lite_runtime 

type file_options = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 20 fields *)
  mutable java_package : string;
  mutable java_outer_classname : string;
  mutable java_multiple_files : bool;
  mutable java_generate_equals_and_hash : bool;
  mutable java_string_check_utf8 : bool;
  mutable optimize_for : file_options_optimize_mode;
  mutable go_package : string;
  mutable cc_generic_services : bool;
  mutable java_generic_services : bool;
  mutable py_generic_services : bool;
  mutable php_generic_services : bool;
  mutable deprecated : bool;
  mutable cc_enable_arenas : bool;
  mutable objc_class_prefix : string;
  mutable csharp_namespace : string;
  mutable swift_prefix : string;
  mutable php_class_prefix : string;
  mutable php_namespace : string;
  mutable php_metadata_namespace : string;
  mutable ruby_package : string;
  mutable uninterpreted_option : uninterpreted_option list;
}

type source_code_info_location = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable path : int32 list;
  mutable span : int32 list;
  mutable leading_comments : string;
  mutable trailing_comments : string;
  mutable leading_detached_comments : string list;
}

type source_code_info = {
  mutable location : source_code_info_location list;
}

type file_descriptor_proto = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable name : string;
  mutable package : string;
  mutable dependency : string list;
  mutable public_dependency : int32 list;
  mutable weak_dependency : int32 list;
  mutable message_type : descriptor_proto list;
  mutable enum_type : enum_descriptor_proto list;
  mutable service : service_descriptor_proto list;
  mutable extension : field_descriptor_proto list;
  mutable options : file_options option;
  mutable source_code_info : source_code_info option;
  mutable syntax : string;
}

type file_descriptor_set = {
  mutable file : file_descriptor_proto list;
}

type generated_code_info_annotation = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable path : int32 list;
  mutable source_file : string;
  mutable begin_ : int32;
  mutable end_ : int32;
}

type generated_code_info = {
  mutable annotation : generated_code_info_annotation list;
}

let default_field_descriptor_proto_label () = (Label_optional:field_descriptor_proto_label)

let default_field_descriptor_proto_type () = (Type_double:field_descriptor_proto_type)

let default_field_options_ctype () = (String:field_options_ctype)

let default_field_options_jstype () = (Js_normal:field_options_jstype)

let default_uninterpreted_option_name_part (): uninterpreted_option_name_part =
{
  name_part="";
  is_extension=false;
}

let default_uninterpreted_option (): uninterpreted_option =
{
  _presence=Pbrt.Bitfield.empty;
  name=[];
  identifier_value="";
  positive_int_value=0L;
  negative_int_value=0L;
  double_value=0.;
  string_value=Bytes.create 0;
  aggregate_value="";
}

let default_field_options (): field_options =
{
  _presence=Pbrt.Bitfield.empty;
  ctype=default_field_options_ctype ();
  packed=false;
  jstype=default_field_options_jstype ();
  lazy_=false;
  unverified_lazy=false;
  deprecated=false;
  weak=false;
  uninterpreted_option=[];
}

let default_field_descriptor_proto (): field_descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  number=0l;
  label=default_field_descriptor_proto_label ();
  type_=default_field_descriptor_proto_type ();
  type_name="";
  extendee="";
  default_value="";
  oneof_index=0l;
  json_name="";
  options=None;
  proto3_optional=false;
}

let default_enum_value_options (): enum_value_options =
{
  _presence=Pbrt.Bitfield.empty;
  deprecated=false;
  uninterpreted_option=[];
}

let default_enum_value_descriptor_proto (): enum_value_descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  number=0l;
  options=None;
}

let default_enum_options (): enum_options =
{
  _presence=Pbrt.Bitfield.empty;
  allow_alias=false;
  deprecated=false;
  uninterpreted_option=[];
}

let default_enum_descriptor_proto_enum_reserved_range (): enum_descriptor_proto_enum_reserved_range =
{
  _presence=Pbrt.Bitfield.empty;
  start=0l;
  end_=0l;
}

let default_enum_descriptor_proto (): enum_descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  value=[];
  options=None;
  reserved_range=[];
  reserved_name=[];
}

let default_extension_range_options (): extension_range_options =
{
  uninterpreted_option=[];
}

let default_descriptor_proto_extension_range (): descriptor_proto_extension_range =
{
  _presence=Pbrt.Bitfield.empty;
  start=0l;
  end_=0l;
  options=None;
}

let default_oneof_options (): oneof_options =
{
  uninterpreted_option=[];
}

let default_oneof_descriptor_proto (): oneof_descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  options=None;
}

let default_message_options (): message_options =
{
  _presence=Pbrt.Bitfield.empty;
  message_set_wire_format=false;
  no_standard_descriptor_accessor=false;
  deprecated=false;
  map_entry=false;
  uninterpreted_option=[];
}

let default_descriptor_proto_reserved_range (): descriptor_proto_reserved_range =
{
  _presence=Pbrt.Bitfield.empty;
  start=0l;
  end_=0l;
}

let default_descriptor_proto (): descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  field=[];
  extension=[];
  nested_type=[];
  enum_type=[];
  extension_range=[];
  oneof_decl=[];
  options=None;
  reserved_range=[];
  reserved_name=[];
}

let default_method_options_idempotency_level () = (Idempotency_unknown:method_options_idempotency_level)

let default_method_options (): method_options =
{
  _presence=Pbrt.Bitfield.empty;
  deprecated=false;
  idempotency_level=default_method_options_idempotency_level ();
  uninterpreted_option=[];
}

let default_method_descriptor_proto (): method_descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  input_type="";
  output_type="";
  options=None;
  client_streaming=false;
  server_streaming=false;
}

let default_service_options (): service_options =
{
  _presence=Pbrt.Bitfield.empty;
  deprecated=false;
  uninterpreted_option=[];
}

let default_service_descriptor_proto (): service_descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  method_=[];
  options=None;
}

let default_file_options_optimize_mode () = (Speed:file_options_optimize_mode)

let default_file_options (): file_options =
{
  _presence=Pbrt.Bitfield.empty;
  java_package="";
  java_outer_classname="";
  java_multiple_files=false;
  java_generate_equals_and_hash=false;
  java_string_check_utf8=false;
  optimize_for=default_file_options_optimize_mode ();
  go_package="";
  cc_generic_services=false;
  java_generic_services=false;
  py_generic_services=false;
  php_generic_services=false;
  deprecated=false;
  cc_enable_arenas=false;
  objc_class_prefix="";
  csharp_namespace="";
  swift_prefix="";
  php_class_prefix="";
  php_namespace="";
  php_metadata_namespace="";
  ruby_package="";
  uninterpreted_option=[];
}

let default_source_code_info_location (): source_code_info_location =
{
  _presence=Pbrt.Bitfield.empty;
  path=[];
  span=[];
  leading_comments="";
  trailing_comments="";
  leading_detached_comments=[];
}

let default_source_code_info (): source_code_info =
{
  location=[];
}

let default_file_descriptor_proto (): file_descriptor_proto =
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  package="";
  dependency=[];
  public_dependency=[];
  weak_dependency=[];
  message_type=[];
  enum_type=[];
  service=[];
  extension=[];
  options=None;
  source_code_info=None;
  syntax="";
}

let default_file_descriptor_set (): file_descriptor_set =
{
  file=[];
}

let default_generated_code_info_annotation (): generated_code_info_annotation =
{
  _presence=Pbrt.Bitfield.empty;
  path=[];
  source_file="";
  begin_=0l;
  end_=0l;
}

let default_generated_code_info (): generated_code_info =
{
  annotation=[];
}


(** {2 Make functions} *)


let[@inline] uninterpreted_option_name_part_set_name_part (self:uninterpreted_option_name_part) (x:string) : unit =
  self.name_part <- x
let[@inline] uninterpreted_option_name_part_set_is_extension (self:uninterpreted_option_name_part) (x:bool) : unit =
  self.is_extension <- x

let copy_uninterpreted_option_name_part (self:uninterpreted_option_name_part) : uninterpreted_option_name_part =
  { self with name_part = self.name_part }

let make_uninterpreted_option_name_part 
  ~(name_part:string) 
  ~(is_extension:bool) 
  () : uninterpreted_option_name_part  =
  let _res = default_uninterpreted_option_name_part () in
  uninterpreted_option_name_part_set_name_part _res name_part;
  uninterpreted_option_name_part_set_is_extension _res is_extension;
  _res

let[@inline] uninterpreted_option_has_identifier_value (self:uninterpreted_option) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] uninterpreted_option_has_positive_int_value (self:uninterpreted_option) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] uninterpreted_option_has_negative_int_value (self:uninterpreted_option) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] uninterpreted_option_has_double_value (self:uninterpreted_option) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] uninterpreted_option_has_string_value (self:uninterpreted_option) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] uninterpreted_option_has_aggregate_value (self:uninterpreted_option) : bool = (Pbrt.Bitfield.get self._presence 5)

let[@inline] uninterpreted_option_set_name (self:uninterpreted_option) (x:uninterpreted_option_name_part list) : unit =
  self.name <- x
let[@inline] uninterpreted_option_set_identifier_value (self:uninterpreted_option) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.identifier_value <- x
let[@inline] uninterpreted_option_set_positive_int_value (self:uninterpreted_option) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.positive_int_value <- x
let[@inline] uninterpreted_option_set_negative_int_value (self:uninterpreted_option) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.negative_int_value <- x
let[@inline] uninterpreted_option_set_double_value (self:uninterpreted_option) (x:float) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.double_value <- x
let[@inline] uninterpreted_option_set_string_value (self:uninterpreted_option) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.string_value <- x
let[@inline] uninterpreted_option_set_aggregate_value (self:uninterpreted_option) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.aggregate_value <- x

let copy_uninterpreted_option (self:uninterpreted_option) : uninterpreted_option =
  { self with name = self.name }

let make_uninterpreted_option 
  ?(name=[])
  ?(identifier_value:string option)
  ?(positive_int_value:int64 option)
  ?(negative_int_value:int64 option)
  ?(double_value:float option)
  ?(string_value:bytes option)
  ?(aggregate_value:string option)
  () : uninterpreted_option  =
  let _res = default_uninterpreted_option () in
  uninterpreted_option_set_name _res name;
  (match identifier_value with
  | None -> ()
  | Some v -> uninterpreted_option_set_identifier_value _res v);
  (match positive_int_value with
  | None -> ()
  | Some v -> uninterpreted_option_set_positive_int_value _res v);
  (match negative_int_value with
  | None -> ()
  | Some v -> uninterpreted_option_set_negative_int_value _res v);
  (match double_value with
  | None -> ()
  | Some v -> uninterpreted_option_set_double_value _res v);
  (match string_value with
  | None -> ()
  | Some v -> uninterpreted_option_set_string_value _res v);
  (match aggregate_value with
  | None -> ()
  | Some v -> uninterpreted_option_set_aggregate_value _res v);
  _res

let[@inline] field_options_has_ctype (self:field_options) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] field_options_has_packed (self:field_options) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] field_options_has_jstype (self:field_options) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] field_options_has_lazy_ (self:field_options) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] field_options_has_unverified_lazy (self:field_options) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] field_options_has_deprecated (self:field_options) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] field_options_has_weak (self:field_options) : bool = (Pbrt.Bitfield.get self._presence 6)

let[@inline] field_options_set_ctype (self:field_options) (x:field_options_ctype) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.ctype <- x
let[@inline] field_options_set_packed (self:field_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.packed <- x
let[@inline] field_options_set_jstype (self:field_options) (x:field_options_jstype) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.jstype <- x
let[@inline] field_options_set_lazy_ (self:field_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.lazy_ <- x
let[@inline] field_options_set_unverified_lazy (self:field_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.unverified_lazy <- x
let[@inline] field_options_set_deprecated (self:field_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.deprecated <- x
let[@inline] field_options_set_weak (self:field_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.weak <- x
let[@inline] field_options_set_uninterpreted_option (self:field_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_field_options (self:field_options) : field_options =
  { self with ctype = self.ctype }

let make_field_options 
  ?(ctype:field_options_ctype option)
  ?(packed:bool option)
  ?(jstype:field_options_jstype option)
  ?(lazy_:bool option)
  ?(unverified_lazy:bool option)
  ?(deprecated:bool option)
  ?(weak:bool option)
  ?(uninterpreted_option=[])
  () : field_options  =
  let _res = default_field_options () in
  (match ctype with
  | None -> ()
  | Some v -> field_options_set_ctype _res v);
  (match packed with
  | None -> ()
  | Some v -> field_options_set_packed _res v);
  (match jstype with
  | None -> ()
  | Some v -> field_options_set_jstype _res v);
  (match lazy_ with
  | None -> ()
  | Some v -> field_options_set_lazy_ _res v);
  (match unverified_lazy with
  | None -> ()
  | Some v -> field_options_set_unverified_lazy _res v);
  (match deprecated with
  | None -> ()
  | Some v -> field_options_set_deprecated _res v);
  (match weak with
  | None -> ()
  | Some v -> field_options_set_weak _res v);
  field_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] field_descriptor_proto_has_name (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] field_descriptor_proto_has_number (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] field_descriptor_proto_has_label (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] field_descriptor_proto_has_type_ (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] field_descriptor_proto_has_type_name (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] field_descriptor_proto_has_extendee (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] field_descriptor_proto_has_default_value (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 6)
let[@inline] field_descriptor_proto_has_oneof_index (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 7)
let[@inline] field_descriptor_proto_has_json_name (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 8)
let[@inline] field_descriptor_proto_has_options (self:field_descriptor_proto) : bool = self.options != None
let[@inline] field_descriptor_proto_has_proto3_optional (self:field_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 9)

let[@inline] field_descriptor_proto_set_name (self:field_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] field_descriptor_proto_set_number (self:field_descriptor_proto) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.number <- x
let[@inline] field_descriptor_proto_set_label (self:field_descriptor_proto) (x:field_descriptor_proto_label) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.label <- x
let[@inline] field_descriptor_proto_set_type_ (self:field_descriptor_proto) (x:field_descriptor_proto_type) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.type_ <- x
let[@inline] field_descriptor_proto_set_type_name (self:field_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.type_name <- x
let[@inline] field_descriptor_proto_set_extendee (self:field_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.extendee <- x
let[@inline] field_descriptor_proto_set_default_value (self:field_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.default_value <- x
let[@inline] field_descriptor_proto_set_oneof_index (self:field_descriptor_proto) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 7); self.oneof_index <- x
let[@inline] field_descriptor_proto_set_json_name (self:field_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 8); self.json_name <- x
let[@inline] field_descriptor_proto_set_options (self:field_descriptor_proto) (x:field_options) : unit =
  self.options <- Some x
let[@inline] field_descriptor_proto_set_proto3_optional (self:field_descriptor_proto) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 9); self.proto3_optional <- x

let copy_field_descriptor_proto (self:field_descriptor_proto) : field_descriptor_proto =
  { self with name = self.name }

let make_field_descriptor_proto 
  ?(name:string option)
  ?(number:int32 option)
  ?(label:field_descriptor_proto_label option)
  ?(type_:field_descriptor_proto_type option)
  ?(type_name:string option)
  ?(extendee:string option)
  ?(default_value:string option)
  ?(oneof_index:int32 option)
  ?(json_name:string option)
  ?(options:field_options option)
  ?(proto3_optional:bool option)
  () : field_descriptor_proto  =
  let _res = default_field_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> field_descriptor_proto_set_name _res v);
  (match number with
  | None -> ()
  | Some v -> field_descriptor_proto_set_number _res v);
  (match label with
  | None -> ()
  | Some v -> field_descriptor_proto_set_label _res v);
  (match type_ with
  | None -> ()
  | Some v -> field_descriptor_proto_set_type_ _res v);
  (match type_name with
  | None -> ()
  | Some v -> field_descriptor_proto_set_type_name _res v);
  (match extendee with
  | None -> ()
  | Some v -> field_descriptor_proto_set_extendee _res v);
  (match default_value with
  | None -> ()
  | Some v -> field_descriptor_proto_set_default_value _res v);
  (match oneof_index with
  | None -> ()
  | Some v -> field_descriptor_proto_set_oneof_index _res v);
  (match json_name with
  | None -> ()
  | Some v -> field_descriptor_proto_set_json_name _res v);
  (match options with
  | None -> ()
  | Some v -> field_descriptor_proto_set_options _res v);
  (match proto3_optional with
  | None -> ()
  | Some v -> field_descriptor_proto_set_proto3_optional _res v);
  _res

let[@inline] enum_value_options_has_deprecated (self:enum_value_options) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] enum_value_options_set_deprecated (self:enum_value_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.deprecated <- x
let[@inline] enum_value_options_set_uninterpreted_option (self:enum_value_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_enum_value_options (self:enum_value_options) : enum_value_options =
  { self with deprecated = self.deprecated }

let make_enum_value_options 
  ?(deprecated:bool option)
  ?(uninterpreted_option=[])
  () : enum_value_options  =
  let _res = default_enum_value_options () in
  (match deprecated with
  | None -> ()
  | Some v -> enum_value_options_set_deprecated _res v);
  enum_value_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] enum_value_descriptor_proto_has_name (self:enum_value_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] enum_value_descriptor_proto_has_number (self:enum_value_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] enum_value_descriptor_proto_has_options (self:enum_value_descriptor_proto) : bool = self.options != None

let[@inline] enum_value_descriptor_proto_set_name (self:enum_value_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] enum_value_descriptor_proto_set_number (self:enum_value_descriptor_proto) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.number <- x
let[@inline] enum_value_descriptor_proto_set_options (self:enum_value_descriptor_proto) (x:enum_value_options) : unit =
  self.options <- Some x

let copy_enum_value_descriptor_proto (self:enum_value_descriptor_proto) : enum_value_descriptor_proto =
  { self with name = self.name }

let make_enum_value_descriptor_proto 
  ?(name:string option)
  ?(number:int32 option)
  ?(options:enum_value_options option)
  () : enum_value_descriptor_proto  =
  let _res = default_enum_value_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> enum_value_descriptor_proto_set_name _res v);
  (match number with
  | None -> ()
  | Some v -> enum_value_descriptor_proto_set_number _res v);
  (match options with
  | None -> ()
  | Some v -> enum_value_descriptor_proto_set_options _res v);
  _res

let[@inline] enum_options_has_allow_alias (self:enum_options) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] enum_options_has_deprecated (self:enum_options) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] enum_options_set_allow_alias (self:enum_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.allow_alias <- x
let[@inline] enum_options_set_deprecated (self:enum_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.deprecated <- x
let[@inline] enum_options_set_uninterpreted_option (self:enum_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_enum_options (self:enum_options) : enum_options =
  { self with allow_alias = self.allow_alias }

let make_enum_options 
  ?(allow_alias:bool option)
  ?(deprecated:bool option)
  ?(uninterpreted_option=[])
  () : enum_options  =
  let _res = default_enum_options () in
  (match allow_alias with
  | None -> ()
  | Some v -> enum_options_set_allow_alias _res v);
  (match deprecated with
  | None -> ()
  | Some v -> enum_options_set_deprecated _res v);
  enum_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] enum_descriptor_proto_enum_reserved_range_has_start (self:enum_descriptor_proto_enum_reserved_range) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] enum_descriptor_proto_enum_reserved_range_has_end_ (self:enum_descriptor_proto_enum_reserved_range) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] enum_descriptor_proto_enum_reserved_range_set_start (self:enum_descriptor_proto_enum_reserved_range) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.start <- x
let[@inline] enum_descriptor_proto_enum_reserved_range_set_end_ (self:enum_descriptor_proto_enum_reserved_range) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.end_ <- x

let copy_enum_descriptor_proto_enum_reserved_range (self:enum_descriptor_proto_enum_reserved_range) : enum_descriptor_proto_enum_reserved_range =
  { self with start = self.start }

let make_enum_descriptor_proto_enum_reserved_range 
  ?(start:int32 option)
  ?(end_:int32 option)
  () : enum_descriptor_proto_enum_reserved_range  =
  let _res = default_enum_descriptor_proto_enum_reserved_range () in
  (match start with
  | None -> ()
  | Some v -> enum_descriptor_proto_enum_reserved_range_set_start _res v);
  (match end_ with
  | None -> ()
  | Some v -> enum_descriptor_proto_enum_reserved_range_set_end_ _res v);
  _res

let[@inline] enum_descriptor_proto_has_name (self:enum_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] enum_descriptor_proto_has_options (self:enum_descriptor_proto) : bool = self.options != None

let[@inline] enum_descriptor_proto_set_name (self:enum_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] enum_descriptor_proto_set_value (self:enum_descriptor_proto) (x:enum_value_descriptor_proto list) : unit =
  self.value <- x
let[@inline] enum_descriptor_proto_set_options (self:enum_descriptor_proto) (x:enum_options) : unit =
  self.options <- Some x
let[@inline] enum_descriptor_proto_set_reserved_range (self:enum_descriptor_proto) (x:enum_descriptor_proto_enum_reserved_range list) : unit =
  self.reserved_range <- x
let[@inline] enum_descriptor_proto_set_reserved_name (self:enum_descriptor_proto) (x:string list) : unit =
  self.reserved_name <- x

let copy_enum_descriptor_proto (self:enum_descriptor_proto) : enum_descriptor_proto =
  { self with name = self.name }

let make_enum_descriptor_proto 
  ?(name:string option)
  ?(value=[])
  ?(options:enum_options option)
  ?(reserved_range=[])
  ?(reserved_name=[])
  () : enum_descriptor_proto  =
  let _res = default_enum_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> enum_descriptor_proto_set_name _res v);
  enum_descriptor_proto_set_value _res value;
  (match options with
  | None -> ()
  | Some v -> enum_descriptor_proto_set_options _res v);
  enum_descriptor_proto_set_reserved_range _res reserved_range;
  enum_descriptor_proto_set_reserved_name _res reserved_name;
  _res


let[@inline] extension_range_options_set_uninterpreted_option (self:extension_range_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_extension_range_options (self:extension_range_options) : extension_range_options =
  { self with uninterpreted_option = self.uninterpreted_option }

let make_extension_range_options 
  ?(uninterpreted_option=[])
  () : extension_range_options  =
  let _res = default_extension_range_options () in
  extension_range_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] descriptor_proto_extension_range_has_start (self:descriptor_proto_extension_range) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] descriptor_proto_extension_range_has_end_ (self:descriptor_proto_extension_range) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] descriptor_proto_extension_range_has_options (self:descriptor_proto_extension_range) : bool = self.options != None

let[@inline] descriptor_proto_extension_range_set_start (self:descriptor_proto_extension_range) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.start <- x
let[@inline] descriptor_proto_extension_range_set_end_ (self:descriptor_proto_extension_range) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.end_ <- x
let[@inline] descriptor_proto_extension_range_set_options (self:descriptor_proto_extension_range) (x:extension_range_options) : unit =
  self.options <- Some x

let copy_descriptor_proto_extension_range (self:descriptor_proto_extension_range) : descriptor_proto_extension_range =
  { self with start = self.start }

let make_descriptor_proto_extension_range 
  ?(start:int32 option)
  ?(end_:int32 option)
  ?(options:extension_range_options option)
  () : descriptor_proto_extension_range  =
  let _res = default_descriptor_proto_extension_range () in
  (match start with
  | None -> ()
  | Some v -> descriptor_proto_extension_range_set_start _res v);
  (match end_ with
  | None -> ()
  | Some v -> descriptor_proto_extension_range_set_end_ _res v);
  (match options with
  | None -> ()
  | Some v -> descriptor_proto_extension_range_set_options _res v);
  _res


let[@inline] oneof_options_set_uninterpreted_option (self:oneof_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_oneof_options (self:oneof_options) : oneof_options =
  { self with uninterpreted_option = self.uninterpreted_option }

let make_oneof_options 
  ?(uninterpreted_option=[])
  () : oneof_options  =
  let _res = default_oneof_options () in
  oneof_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] oneof_descriptor_proto_has_name (self:oneof_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] oneof_descriptor_proto_has_options (self:oneof_descriptor_proto) : bool = self.options != None

let[@inline] oneof_descriptor_proto_set_name (self:oneof_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] oneof_descriptor_proto_set_options (self:oneof_descriptor_proto) (x:oneof_options) : unit =
  self.options <- Some x

let copy_oneof_descriptor_proto (self:oneof_descriptor_proto) : oneof_descriptor_proto =
  { self with name = self.name }

let make_oneof_descriptor_proto 
  ?(name:string option)
  ?(options:oneof_options option)
  () : oneof_descriptor_proto  =
  let _res = default_oneof_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> oneof_descriptor_proto_set_name _res v);
  (match options with
  | None -> ()
  | Some v -> oneof_descriptor_proto_set_options _res v);
  _res

let[@inline] message_options_has_message_set_wire_format (self:message_options) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] message_options_has_no_standard_descriptor_accessor (self:message_options) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] message_options_has_deprecated (self:message_options) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] message_options_has_map_entry (self:message_options) : bool = (Pbrt.Bitfield.get self._presence 3)

let[@inline] message_options_set_message_set_wire_format (self:message_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.message_set_wire_format <- x
let[@inline] message_options_set_no_standard_descriptor_accessor (self:message_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.no_standard_descriptor_accessor <- x
let[@inline] message_options_set_deprecated (self:message_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.deprecated <- x
let[@inline] message_options_set_map_entry (self:message_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.map_entry <- x
let[@inline] message_options_set_uninterpreted_option (self:message_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_message_options (self:message_options) : message_options =
  { self with message_set_wire_format = self.message_set_wire_format }

let make_message_options 
  ?(message_set_wire_format:bool option)
  ?(no_standard_descriptor_accessor:bool option)
  ?(deprecated:bool option)
  ?(map_entry:bool option)
  ?(uninterpreted_option=[])
  () : message_options  =
  let _res = default_message_options () in
  (match message_set_wire_format with
  | None -> ()
  | Some v -> message_options_set_message_set_wire_format _res v);
  (match no_standard_descriptor_accessor with
  | None -> ()
  | Some v -> message_options_set_no_standard_descriptor_accessor _res v);
  (match deprecated with
  | None -> ()
  | Some v -> message_options_set_deprecated _res v);
  (match map_entry with
  | None -> ()
  | Some v -> message_options_set_map_entry _res v);
  message_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] descriptor_proto_reserved_range_has_start (self:descriptor_proto_reserved_range) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] descriptor_proto_reserved_range_has_end_ (self:descriptor_proto_reserved_range) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] descriptor_proto_reserved_range_set_start (self:descriptor_proto_reserved_range) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.start <- x
let[@inline] descriptor_proto_reserved_range_set_end_ (self:descriptor_proto_reserved_range) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.end_ <- x

let copy_descriptor_proto_reserved_range (self:descriptor_proto_reserved_range) : descriptor_proto_reserved_range =
  { self with start = self.start }

let make_descriptor_proto_reserved_range 
  ?(start:int32 option)
  ?(end_:int32 option)
  () : descriptor_proto_reserved_range  =
  let _res = default_descriptor_proto_reserved_range () in
  (match start with
  | None -> ()
  | Some v -> descriptor_proto_reserved_range_set_start _res v);
  (match end_ with
  | None -> ()
  | Some v -> descriptor_proto_reserved_range_set_end_ _res v);
  _res

let[@inline] descriptor_proto_has_name (self:descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] descriptor_proto_has_options (self:descriptor_proto) : bool = self.options != None

let[@inline] descriptor_proto_set_name (self:descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] descriptor_proto_set_field (self:descriptor_proto) (x:field_descriptor_proto list) : unit =
  self.field <- x
let[@inline] descriptor_proto_set_extension (self:descriptor_proto) (x:field_descriptor_proto list) : unit =
  self.extension <- x
let[@inline] descriptor_proto_set_nested_type (self:descriptor_proto) (x:descriptor_proto list) : unit =
  self.nested_type <- x
let[@inline] descriptor_proto_set_enum_type (self:descriptor_proto) (x:enum_descriptor_proto list) : unit =
  self.enum_type <- x
let[@inline] descriptor_proto_set_extension_range (self:descriptor_proto) (x:descriptor_proto_extension_range list) : unit =
  self.extension_range <- x
let[@inline] descriptor_proto_set_oneof_decl (self:descriptor_proto) (x:oneof_descriptor_proto list) : unit =
  self.oneof_decl <- x
let[@inline] descriptor_proto_set_options (self:descriptor_proto) (x:message_options) : unit =
  self.options <- Some x
let[@inline] descriptor_proto_set_reserved_range (self:descriptor_proto) (x:descriptor_proto_reserved_range list) : unit =
  self.reserved_range <- x
let[@inline] descriptor_proto_set_reserved_name (self:descriptor_proto) (x:string list) : unit =
  self.reserved_name <- x

let copy_descriptor_proto (self:descriptor_proto) : descriptor_proto =
  { self with name = self.name }

let make_descriptor_proto 
  ?(name:string option)
  ?(field=[])
  ?(extension=[])
  ?(nested_type=[])
  ?(enum_type=[])
  ?(extension_range=[])
  ?(oneof_decl=[])
  ?(options:message_options option)
  ?(reserved_range=[])
  ?(reserved_name=[])
  () : descriptor_proto  =
  let _res = default_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> descriptor_proto_set_name _res v);
  descriptor_proto_set_field _res field;
  descriptor_proto_set_extension _res extension;
  descriptor_proto_set_nested_type _res nested_type;
  descriptor_proto_set_enum_type _res enum_type;
  descriptor_proto_set_extension_range _res extension_range;
  descriptor_proto_set_oneof_decl _res oneof_decl;
  (match options with
  | None -> ()
  | Some v -> descriptor_proto_set_options _res v);
  descriptor_proto_set_reserved_range _res reserved_range;
  descriptor_proto_set_reserved_name _res reserved_name;
  _res

let[@inline] method_options_has_deprecated (self:method_options) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] method_options_has_idempotency_level (self:method_options) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] method_options_set_deprecated (self:method_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.deprecated <- x
let[@inline] method_options_set_idempotency_level (self:method_options) (x:method_options_idempotency_level) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.idempotency_level <- x
let[@inline] method_options_set_uninterpreted_option (self:method_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_method_options (self:method_options) : method_options =
  { self with deprecated = self.deprecated }

let make_method_options 
  ?(deprecated:bool option)
  ?(idempotency_level:method_options_idempotency_level option)
  ?(uninterpreted_option=[])
  () : method_options  =
  let _res = default_method_options () in
  (match deprecated with
  | None -> ()
  | Some v -> method_options_set_deprecated _res v);
  (match idempotency_level with
  | None -> ()
  | Some v -> method_options_set_idempotency_level _res v);
  method_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] method_descriptor_proto_has_name (self:method_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] method_descriptor_proto_has_input_type (self:method_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] method_descriptor_proto_has_output_type (self:method_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] method_descriptor_proto_has_options (self:method_descriptor_proto) : bool = self.options != None
let[@inline] method_descriptor_proto_has_client_streaming (self:method_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] method_descriptor_proto_has_server_streaming (self:method_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 4)

let[@inline] method_descriptor_proto_set_name (self:method_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] method_descriptor_proto_set_input_type (self:method_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.input_type <- x
let[@inline] method_descriptor_proto_set_output_type (self:method_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.output_type <- x
let[@inline] method_descriptor_proto_set_options (self:method_descriptor_proto) (x:method_options) : unit =
  self.options <- Some x
let[@inline] method_descriptor_proto_set_client_streaming (self:method_descriptor_proto) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.client_streaming <- x
let[@inline] method_descriptor_proto_set_server_streaming (self:method_descriptor_proto) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.server_streaming <- x

let copy_method_descriptor_proto (self:method_descriptor_proto) : method_descriptor_proto =
  { self with name = self.name }

let make_method_descriptor_proto 
  ?(name:string option)
  ?(input_type:string option)
  ?(output_type:string option)
  ?(options:method_options option)
  ?(client_streaming:bool option)
  ?(server_streaming:bool option)
  () : method_descriptor_proto  =
  let _res = default_method_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> method_descriptor_proto_set_name _res v);
  (match input_type with
  | None -> ()
  | Some v -> method_descriptor_proto_set_input_type _res v);
  (match output_type with
  | None -> ()
  | Some v -> method_descriptor_proto_set_output_type _res v);
  (match options with
  | None -> ()
  | Some v -> method_descriptor_proto_set_options _res v);
  (match client_streaming with
  | None -> ()
  | Some v -> method_descriptor_proto_set_client_streaming _res v);
  (match server_streaming with
  | None -> ()
  | Some v -> method_descriptor_proto_set_server_streaming _res v);
  _res

let[@inline] service_options_has_deprecated (self:service_options) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] service_options_set_deprecated (self:service_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.deprecated <- x
let[@inline] service_options_set_uninterpreted_option (self:service_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_service_options (self:service_options) : service_options =
  { self with deprecated = self.deprecated }

let make_service_options 
  ?(deprecated:bool option)
  ?(uninterpreted_option=[])
  () : service_options  =
  let _res = default_service_options () in
  (match deprecated with
  | None -> ()
  | Some v -> service_options_set_deprecated _res v);
  service_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] service_descriptor_proto_has_name (self:service_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] service_descriptor_proto_has_options (self:service_descriptor_proto) : bool = self.options != None

let[@inline] service_descriptor_proto_set_name (self:service_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] service_descriptor_proto_set_method_ (self:service_descriptor_proto) (x:method_descriptor_proto list) : unit =
  self.method_ <- x
let[@inline] service_descriptor_proto_set_options (self:service_descriptor_proto) (x:service_options) : unit =
  self.options <- Some x

let copy_service_descriptor_proto (self:service_descriptor_proto) : service_descriptor_proto =
  { self with name = self.name }

let make_service_descriptor_proto 
  ?(name:string option)
  ?(method_=[])
  ?(options:service_options option)
  () : service_descriptor_proto  =
  let _res = default_service_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> service_descriptor_proto_set_name _res v);
  service_descriptor_proto_set_method_ _res method_;
  (match options with
  | None -> ()
  | Some v -> service_descriptor_proto_set_options _res v);
  _res

let[@inline] file_options_has_java_package (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] file_options_has_java_outer_classname (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] file_options_has_java_multiple_files (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] file_options_has_java_generate_equals_and_hash (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] file_options_has_java_string_check_utf8 (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] file_options_has_optimize_for (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] file_options_has_go_package (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 6)
let[@inline] file_options_has_cc_generic_services (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 7)
let[@inline] file_options_has_java_generic_services (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 8)
let[@inline] file_options_has_py_generic_services (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 9)
let[@inline] file_options_has_php_generic_services (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 10)
let[@inline] file_options_has_deprecated (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 11)
let[@inline] file_options_has_cc_enable_arenas (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 12)
let[@inline] file_options_has_objc_class_prefix (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 13)
let[@inline] file_options_has_csharp_namespace (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 14)
let[@inline] file_options_has_swift_prefix (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 15)
let[@inline] file_options_has_php_class_prefix (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 16)
let[@inline] file_options_has_php_namespace (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 17)
let[@inline] file_options_has_php_metadata_namespace (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 18)
let[@inline] file_options_has_ruby_package (self:file_options) : bool = (Pbrt.Bitfield.get self._presence 19)

let[@inline] file_options_set_java_package (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.java_package <- x
let[@inline] file_options_set_java_outer_classname (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.java_outer_classname <- x
let[@inline] file_options_set_java_multiple_files (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.java_multiple_files <- x
let[@inline] file_options_set_java_generate_equals_and_hash (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.java_generate_equals_and_hash <- x
let[@inline] file_options_set_java_string_check_utf8 (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.java_string_check_utf8 <- x
let[@inline] file_options_set_optimize_for (self:file_options) (x:file_options_optimize_mode) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.optimize_for <- x
let[@inline] file_options_set_go_package (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.go_package <- x
let[@inline] file_options_set_cc_generic_services (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 7); self.cc_generic_services <- x
let[@inline] file_options_set_java_generic_services (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 8); self.java_generic_services <- x
let[@inline] file_options_set_py_generic_services (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 9); self.py_generic_services <- x
let[@inline] file_options_set_php_generic_services (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 10); self.php_generic_services <- x
let[@inline] file_options_set_deprecated (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 11); self.deprecated <- x
let[@inline] file_options_set_cc_enable_arenas (self:file_options) (x:bool) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 12); self.cc_enable_arenas <- x
let[@inline] file_options_set_objc_class_prefix (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 13); self.objc_class_prefix <- x
let[@inline] file_options_set_csharp_namespace (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 14); self.csharp_namespace <- x
let[@inline] file_options_set_swift_prefix (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 15); self.swift_prefix <- x
let[@inline] file_options_set_php_class_prefix (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 16); self.php_class_prefix <- x
let[@inline] file_options_set_php_namespace (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 17); self.php_namespace <- x
let[@inline] file_options_set_php_metadata_namespace (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 18); self.php_metadata_namespace <- x
let[@inline] file_options_set_ruby_package (self:file_options) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 19); self.ruby_package <- x
let[@inline] file_options_set_uninterpreted_option (self:file_options) (x:uninterpreted_option list) : unit =
  self.uninterpreted_option <- x

let copy_file_options (self:file_options) : file_options =
  { self with java_package = self.java_package }

let make_file_options 
  ?(java_package:string option)
  ?(java_outer_classname:string option)
  ?(java_multiple_files:bool option)
  ?(java_generate_equals_and_hash:bool option)
  ?(java_string_check_utf8:bool option)
  ?(optimize_for:file_options_optimize_mode option)
  ?(go_package:string option)
  ?(cc_generic_services:bool option)
  ?(java_generic_services:bool option)
  ?(py_generic_services:bool option)
  ?(php_generic_services:bool option)
  ?(deprecated:bool option)
  ?(cc_enable_arenas:bool option)
  ?(objc_class_prefix:string option)
  ?(csharp_namespace:string option)
  ?(swift_prefix:string option)
  ?(php_class_prefix:string option)
  ?(php_namespace:string option)
  ?(php_metadata_namespace:string option)
  ?(ruby_package:string option)
  ?(uninterpreted_option=[])
  () : file_options  =
  let _res = default_file_options () in
  (match java_package with
  | None -> ()
  | Some v -> file_options_set_java_package _res v);
  (match java_outer_classname with
  | None -> ()
  | Some v -> file_options_set_java_outer_classname _res v);
  (match java_multiple_files with
  | None -> ()
  | Some v -> file_options_set_java_multiple_files _res v);
  (match java_generate_equals_and_hash with
  | None -> ()
  | Some v -> file_options_set_java_generate_equals_and_hash _res v);
  (match java_string_check_utf8 with
  | None -> ()
  | Some v -> file_options_set_java_string_check_utf8 _res v);
  (match optimize_for with
  | None -> ()
  | Some v -> file_options_set_optimize_for _res v);
  (match go_package with
  | None -> ()
  | Some v -> file_options_set_go_package _res v);
  (match cc_generic_services with
  | None -> ()
  | Some v -> file_options_set_cc_generic_services _res v);
  (match java_generic_services with
  | None -> ()
  | Some v -> file_options_set_java_generic_services _res v);
  (match py_generic_services with
  | None -> ()
  | Some v -> file_options_set_py_generic_services _res v);
  (match php_generic_services with
  | None -> ()
  | Some v -> file_options_set_php_generic_services _res v);
  (match deprecated with
  | None -> ()
  | Some v -> file_options_set_deprecated _res v);
  (match cc_enable_arenas with
  | None -> ()
  | Some v -> file_options_set_cc_enable_arenas _res v);
  (match objc_class_prefix with
  | None -> ()
  | Some v -> file_options_set_objc_class_prefix _res v);
  (match csharp_namespace with
  | None -> ()
  | Some v -> file_options_set_csharp_namespace _res v);
  (match swift_prefix with
  | None -> ()
  | Some v -> file_options_set_swift_prefix _res v);
  (match php_class_prefix with
  | None -> ()
  | Some v -> file_options_set_php_class_prefix _res v);
  (match php_namespace with
  | None -> ()
  | Some v -> file_options_set_php_namespace _res v);
  (match php_metadata_namespace with
  | None -> ()
  | Some v -> file_options_set_php_metadata_namespace _res v);
  (match ruby_package with
  | None -> ()
  | Some v -> file_options_set_ruby_package _res v);
  file_options_set_uninterpreted_option _res uninterpreted_option;
  _res

let[@inline] source_code_info_location_has_leading_comments (self:source_code_info_location) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] source_code_info_location_has_trailing_comments (self:source_code_info_location) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] source_code_info_location_set_path (self:source_code_info_location) (x:int32 list) : unit =
  self.path <- x
let[@inline] source_code_info_location_set_span (self:source_code_info_location) (x:int32 list) : unit =
  self.span <- x
let[@inline] source_code_info_location_set_leading_comments (self:source_code_info_location) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.leading_comments <- x
let[@inline] source_code_info_location_set_trailing_comments (self:source_code_info_location) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.trailing_comments <- x
let[@inline] source_code_info_location_set_leading_detached_comments (self:source_code_info_location) (x:string list) : unit =
  self.leading_detached_comments <- x

let copy_source_code_info_location (self:source_code_info_location) : source_code_info_location =
  { self with path = self.path }

let make_source_code_info_location 
  ?(path=[])
  ?(span=[])
  ?(leading_comments:string option)
  ?(trailing_comments:string option)
  ?(leading_detached_comments=[])
  () : source_code_info_location  =
  let _res = default_source_code_info_location () in
  source_code_info_location_set_path _res path;
  source_code_info_location_set_span _res span;
  (match leading_comments with
  | None -> ()
  | Some v -> source_code_info_location_set_leading_comments _res v);
  (match trailing_comments with
  | None -> ()
  | Some v -> source_code_info_location_set_trailing_comments _res v);
  source_code_info_location_set_leading_detached_comments _res leading_detached_comments;
  _res


let[@inline] source_code_info_set_location (self:source_code_info) (x:source_code_info_location list) : unit =
  self.location <- x

let copy_source_code_info (self:source_code_info) : source_code_info =
  { self with location = self.location }

let make_source_code_info 
  ?(location=[])
  () : source_code_info  =
  let _res = default_source_code_info () in
  source_code_info_set_location _res location;
  _res

let[@inline] file_descriptor_proto_has_name (self:file_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] file_descriptor_proto_has_package (self:file_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] file_descriptor_proto_has_options (self:file_descriptor_proto) : bool = self.options != None
let[@inline] file_descriptor_proto_has_source_code_info (self:file_descriptor_proto) : bool = self.source_code_info != None
let[@inline] file_descriptor_proto_has_syntax (self:file_descriptor_proto) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] file_descriptor_proto_set_name (self:file_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] file_descriptor_proto_set_package (self:file_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.package <- x
let[@inline] file_descriptor_proto_set_dependency (self:file_descriptor_proto) (x:string list) : unit =
  self.dependency <- x
let[@inline] file_descriptor_proto_set_public_dependency (self:file_descriptor_proto) (x:int32 list) : unit =
  self.public_dependency <- x
let[@inline] file_descriptor_proto_set_weak_dependency (self:file_descriptor_proto) (x:int32 list) : unit =
  self.weak_dependency <- x
let[@inline] file_descriptor_proto_set_message_type (self:file_descriptor_proto) (x:descriptor_proto list) : unit =
  self.message_type <- x
let[@inline] file_descriptor_proto_set_enum_type (self:file_descriptor_proto) (x:enum_descriptor_proto list) : unit =
  self.enum_type <- x
let[@inline] file_descriptor_proto_set_service (self:file_descriptor_proto) (x:service_descriptor_proto list) : unit =
  self.service <- x
let[@inline] file_descriptor_proto_set_extension (self:file_descriptor_proto) (x:field_descriptor_proto list) : unit =
  self.extension <- x
let[@inline] file_descriptor_proto_set_options (self:file_descriptor_proto) (x:file_options) : unit =
  self.options <- Some x
let[@inline] file_descriptor_proto_set_source_code_info (self:file_descriptor_proto) (x:source_code_info) : unit =
  self.source_code_info <- Some x
let[@inline] file_descriptor_proto_set_syntax (self:file_descriptor_proto) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.syntax <- x

let copy_file_descriptor_proto (self:file_descriptor_proto) : file_descriptor_proto =
  { self with name = self.name }

let make_file_descriptor_proto 
  ?(name:string option)
  ?(package:string option)
  ?(dependency=[])
  ?(public_dependency=[])
  ?(weak_dependency=[])
  ?(message_type=[])
  ?(enum_type=[])
  ?(service=[])
  ?(extension=[])
  ?(options:file_options option)
  ?(source_code_info:source_code_info option)
  ?(syntax:string option)
  () : file_descriptor_proto  =
  let _res = default_file_descriptor_proto () in
  (match name with
  | None -> ()
  | Some v -> file_descriptor_proto_set_name _res v);
  (match package with
  | None -> ()
  | Some v -> file_descriptor_proto_set_package _res v);
  file_descriptor_proto_set_dependency _res dependency;
  file_descriptor_proto_set_public_dependency _res public_dependency;
  file_descriptor_proto_set_weak_dependency _res weak_dependency;
  file_descriptor_proto_set_message_type _res message_type;
  file_descriptor_proto_set_enum_type _res enum_type;
  file_descriptor_proto_set_service _res service;
  file_descriptor_proto_set_extension _res extension;
  (match options with
  | None -> ()
  | Some v -> file_descriptor_proto_set_options _res v);
  (match source_code_info with
  | None -> ()
  | Some v -> file_descriptor_proto_set_source_code_info _res v);
  (match syntax with
  | None -> ()
  | Some v -> file_descriptor_proto_set_syntax _res v);
  _res


let[@inline] file_descriptor_set_set_file (self:file_descriptor_set) (x:file_descriptor_proto list) : unit =
  self.file <- x

let copy_file_descriptor_set (self:file_descriptor_set) : file_descriptor_set =
  { self with file = self.file }

let make_file_descriptor_set 
  ?(file=[])
  () : file_descriptor_set  =
  let _res = default_file_descriptor_set () in
  file_descriptor_set_set_file _res file;
  _res

let[@inline] generated_code_info_annotation_has_source_file (self:generated_code_info_annotation) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] generated_code_info_annotation_has_begin_ (self:generated_code_info_annotation) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] generated_code_info_annotation_has_end_ (self:generated_code_info_annotation) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] generated_code_info_annotation_set_path (self:generated_code_info_annotation) (x:int32 list) : unit =
  self.path <- x
let[@inline] generated_code_info_annotation_set_source_file (self:generated_code_info_annotation) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.source_file <- x
let[@inline] generated_code_info_annotation_set_begin_ (self:generated_code_info_annotation) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.begin_ <- x
let[@inline] generated_code_info_annotation_set_end_ (self:generated_code_info_annotation) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.end_ <- x

let copy_generated_code_info_annotation (self:generated_code_info_annotation) : generated_code_info_annotation =
  { self with path = self.path }

let make_generated_code_info_annotation 
  ?(path=[])
  ?(source_file:string option)
  ?(begin_:int32 option)
  ?(end_:int32 option)
  () : generated_code_info_annotation  =
  let _res = default_generated_code_info_annotation () in
  generated_code_info_annotation_set_path _res path;
  (match source_file with
  | None -> ()
  | Some v -> generated_code_info_annotation_set_source_file _res v);
  (match begin_ with
  | None -> ()
  | Some v -> generated_code_info_annotation_set_begin_ _res v);
  (match end_ with
  | None -> ()
  | Some v -> generated_code_info_annotation_set_end_ _res v);
  _res


let[@inline] generated_code_info_set_annotation (self:generated_code_info) (x:generated_code_info_annotation list) : unit =
  self.annotation <- x

let copy_generated_code_info (self:generated_code_info) : generated_code_info =
  { self with annotation = self.annotation }

let make_generated_code_info 
  ?(annotation=[])
  () : generated_code_info  =
  let _res = default_generated_code_info () in
  generated_code_info_set_annotation _res annotation;
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_field_descriptor_proto_label (v:field_descriptor_proto_label) encoder =
  match v with
  | Label_optional -> Pbrt.Encoder.int_as_varint 1 encoder
  | Label_required -> Pbrt.Encoder.int_as_varint 2 encoder
  | Label_repeated -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_pb_field_descriptor_proto_type (v:field_descriptor_proto_type) encoder =
  match v with
  | Type_double -> Pbrt.Encoder.int_as_varint 1 encoder
  | Type_float -> Pbrt.Encoder.int_as_varint 2 encoder
  | Type_int64 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Type_uint64 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Type_int32 -> Pbrt.Encoder.int_as_varint 5 encoder
  | Type_fixed64 -> Pbrt.Encoder.int_as_varint 6 encoder
  | Type_fixed32 -> Pbrt.Encoder.int_as_varint 7 encoder
  | Type_bool -> Pbrt.Encoder.int_as_varint 8 encoder
  | Type_string -> Pbrt.Encoder.int_as_varint 9 encoder
  | Type_group -> Pbrt.Encoder.int_as_varint 10 encoder
  | Type_message -> Pbrt.Encoder.int_as_varint 11 encoder
  | Type_bytes -> Pbrt.Encoder.int_as_varint 12 encoder
  | Type_uint32 -> Pbrt.Encoder.int_as_varint 13 encoder
  | Type_enum -> Pbrt.Encoder.int_as_varint 14 encoder
  | Type_sfixed32 -> Pbrt.Encoder.int_as_varint 15 encoder
  | Type_sfixed64 -> Pbrt.Encoder.int_as_varint 16 encoder
  | Type_sint32 -> Pbrt.Encoder.int_as_varint 17 encoder
  | Type_sint64 -> Pbrt.Encoder.int_as_varint 18 encoder

let rec encode_pb_field_options_ctype (v:field_options_ctype) encoder =
  match v with
  | String -> Pbrt.Encoder.int_as_varint (0) encoder
  | Cord -> Pbrt.Encoder.int_as_varint 1 encoder
  | String_piece -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_pb_field_options_jstype (v:field_options_jstype) encoder =
  match v with
  | Js_normal -> Pbrt.Encoder.int_as_varint (0) encoder
  | Js_string -> Pbrt.Encoder.int_as_varint 1 encoder
  | Js_number -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_pb_uninterpreted_option_name_part (v:uninterpreted_option_name_part) encoder = 
  Pbrt.Encoder.string v.name_part encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.bool v.is_extension encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  ()

let rec encode_pb_uninterpreted_option (v:uninterpreted_option) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option_name_part x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.name encoder;
  if uninterpreted_option_has_identifier_value v then (
    Pbrt.Encoder.string v.identifier_value encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  if uninterpreted_option_has_positive_int_value v then (
    Pbrt.Encoder.int64_as_varint v.positive_int_value encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  );
  if uninterpreted_option_has_negative_int_value v then (
    Pbrt.Encoder.int64_as_varint v.negative_int_value encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  );
  if uninterpreted_option_has_double_value v then (
    Pbrt.Encoder.float_as_bits64 v.double_value encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  );
  if uninterpreted_option_has_string_value v then (
    Pbrt.Encoder.bytes v.string_value encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  );
  if uninterpreted_option_has_aggregate_value v then (
    Pbrt.Encoder.string v.aggregate_value encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_field_options (v:field_options) encoder = 
  if field_options_has_ctype v then (
    encode_pb_field_options_ctype v.ctype encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  if field_options_has_packed v then (
    Pbrt.Encoder.bool v.packed encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  if field_options_has_jstype v then (
    encode_pb_field_options_jstype v.jstype encoder;
    Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  );
  if field_options_has_lazy_ v then (
    Pbrt.Encoder.bool v.lazy_ encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  );
  if field_options_has_unverified_lazy v then (
    Pbrt.Encoder.bool v.unverified_lazy encoder;
    Pbrt.Encoder.key 15 Pbrt.Varint encoder; 
  );
  if field_options_has_deprecated v then (
    Pbrt.Encoder.bool v.deprecated encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if field_options_has_weak v then (
    Pbrt.Encoder.bool v.weak encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_field_descriptor_proto (v:field_descriptor_proto) encoder = 
  if field_descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if field_descriptor_proto_has_number v then (
    Pbrt.Encoder.int32_as_varint v.number encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if field_descriptor_proto_has_label v then (
    encode_pb_field_descriptor_proto_label v.label encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  );
  if field_descriptor_proto_has_type_ v then (
    encode_pb_field_descriptor_proto_type v.type_ encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  );
  if field_descriptor_proto_has_type_name v then (
    Pbrt.Encoder.string v.type_name encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  );
  if field_descriptor_proto_has_extendee v then (
    Pbrt.Encoder.string v.extendee encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if field_descriptor_proto_has_default_value v then (
    Pbrt.Encoder.string v.default_value encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  );
  if field_descriptor_proto_has_oneof_index v then (
    Pbrt.Encoder.int32_as_varint v.oneof_index encoder;
    Pbrt.Encoder.key 9 Pbrt.Varint encoder; 
  );
  if field_descriptor_proto_has_json_name v then (
    Pbrt.Encoder.string v.json_name encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  );
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_field_options x encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  if field_descriptor_proto_has_proto3_optional v then (
    Pbrt.Encoder.bool v.proto3_optional encoder;
    Pbrt.Encoder.key 17 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_enum_value_options (v:enum_value_options) encoder = 
  if enum_value_options_has_deprecated v then (
    Pbrt.Encoder.bool v.deprecated encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_enum_value_descriptor_proto (v:enum_value_descriptor_proto) encoder = 
  if enum_value_descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if enum_value_descriptor_proto_has_number v then (
    Pbrt.Encoder.int32_as_varint v.number encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_enum_value_options x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_enum_options (v:enum_options) encoder = 
  if enum_options_has_allow_alias v then (
    Pbrt.Encoder.bool v.allow_alias encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  if enum_options_has_deprecated v then (
    Pbrt.Encoder.bool v.deprecated encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_enum_descriptor_proto_enum_reserved_range (v:enum_descriptor_proto_enum_reserved_range) encoder = 
  if enum_descriptor_proto_enum_reserved_range_has_start v then (
    Pbrt.Encoder.int32_as_varint v.start encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  if enum_descriptor_proto_enum_reserved_range_has_end_ v then (
    Pbrt.Encoder.int32_as_varint v.end_ encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_enum_descriptor_proto (v:enum_descriptor_proto) encoder = 
  if enum_descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_enum_value_descriptor_proto x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.value encoder;
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_enum_options x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_enum_descriptor_proto_enum_reserved_range x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.reserved_range encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.reserved_name encoder;
  ()

let rec encode_pb_extension_range_options (v:extension_range_options) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_descriptor_proto_extension_range (v:descriptor_proto_extension_range) encoder = 
  if descriptor_proto_extension_range_has_start v then (
    Pbrt.Encoder.int32_as_varint v.start encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  if descriptor_proto_extension_range_has_end_ v then (
    Pbrt.Encoder.int32_as_varint v.end_ encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_extension_range_options x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_oneof_options (v:oneof_options) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_oneof_descriptor_proto (v:oneof_descriptor_proto) encoder = 
  if oneof_descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_oneof_options x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_message_options (v:message_options) encoder = 
  if message_options_has_message_set_wire_format v then (
    Pbrt.Encoder.bool v.message_set_wire_format encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  if message_options_has_no_standard_descriptor_accessor v then (
    Pbrt.Encoder.bool v.no_standard_descriptor_accessor encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  if message_options_has_deprecated v then (
    Pbrt.Encoder.bool v.deprecated encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if message_options_has_map_entry v then (
    Pbrt.Encoder.bool v.map_entry encoder;
    Pbrt.Encoder.key 7 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_descriptor_proto_reserved_range (v:descriptor_proto_reserved_range) encoder = 
  if descriptor_proto_reserved_range_has_start v then (
    Pbrt.Encoder.int32_as_varint v.start encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  if descriptor_proto_reserved_range_has_end_ v then (
    Pbrt.Encoder.int32_as_varint v.end_ encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_descriptor_proto (v:descriptor_proto) encoder = 
  if descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_field_descriptor_proto x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.field encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_field_descriptor_proto x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.extension encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_descriptor_proto x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.nested_type encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_enum_descriptor_proto x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.enum_type encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_descriptor_proto_extension_range x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.extension_range encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_oneof_descriptor_proto x encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  ) v.oneof_decl encoder;
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_message_options x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_descriptor_proto_reserved_range x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  ) v.reserved_range encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  ) v.reserved_name encoder;
  ()

let rec encode_pb_method_options_idempotency_level (v:method_options_idempotency_level) encoder =
  match v with
  | Idempotency_unknown -> Pbrt.Encoder.int_as_varint (0) encoder
  | No_side_effects -> Pbrt.Encoder.int_as_varint 1 encoder
  | Idempotent -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_pb_method_options (v:method_options) encoder = 
  if method_options_has_deprecated v then (
    Pbrt.Encoder.bool v.deprecated encoder;
    Pbrt.Encoder.key 33 Pbrt.Varint encoder; 
  );
  if method_options_has_idempotency_level v then (
    encode_pb_method_options_idempotency_level v.idempotency_level encoder;
    Pbrt.Encoder.key 34 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_method_descriptor_proto (v:method_descriptor_proto) encoder = 
  if method_descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if method_descriptor_proto_has_input_type v then (
    Pbrt.Encoder.string v.input_type encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if method_descriptor_proto_has_output_type v then (
    Pbrt.Encoder.string v.output_type encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_method_options x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  if method_descriptor_proto_has_client_streaming v then (
    Pbrt.Encoder.bool v.client_streaming encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  );
  if method_descriptor_proto_has_server_streaming v then (
    Pbrt.Encoder.bool v.server_streaming encoder;
    Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_service_options (v:service_options) encoder = 
  if service_options_has_deprecated v then (
    Pbrt.Encoder.bool v.deprecated encoder;
    Pbrt.Encoder.key 33 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_service_descriptor_proto (v:service_descriptor_proto) encoder = 
  if service_descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_method_descriptor_proto x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.method_ encoder;
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_service_options x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_file_options_optimize_mode (v:file_options_optimize_mode) encoder =
  match v with
  | Speed -> Pbrt.Encoder.int_as_varint 1 encoder
  | Code_size -> Pbrt.Encoder.int_as_varint 2 encoder
  | Lite_runtime -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_pb_file_options (v:file_options) encoder = 
  if file_options_has_java_package v then (
    Pbrt.Encoder.string v.java_package encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if file_options_has_java_outer_classname v then (
    Pbrt.Encoder.string v.java_outer_classname encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  );
  if file_options_has_java_multiple_files v then (
    Pbrt.Encoder.bool v.java_multiple_files encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  );
  if file_options_has_java_generate_equals_and_hash v then (
    Pbrt.Encoder.bool v.java_generate_equals_and_hash encoder;
    Pbrt.Encoder.key 20 Pbrt.Varint encoder; 
  );
  if file_options_has_java_string_check_utf8 v then (
    Pbrt.Encoder.bool v.java_string_check_utf8 encoder;
    Pbrt.Encoder.key 27 Pbrt.Varint encoder; 
  );
  if file_options_has_optimize_for v then (
    encode_pb_file_options_optimize_mode v.optimize_for encoder;
    Pbrt.Encoder.key 9 Pbrt.Varint encoder; 
  );
  if file_options_has_go_package v then (
    Pbrt.Encoder.string v.go_package encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  );
  if file_options_has_cc_generic_services v then (
    Pbrt.Encoder.bool v.cc_generic_services encoder;
    Pbrt.Encoder.key 16 Pbrt.Varint encoder; 
  );
  if file_options_has_java_generic_services v then (
    Pbrt.Encoder.bool v.java_generic_services encoder;
    Pbrt.Encoder.key 17 Pbrt.Varint encoder; 
  );
  if file_options_has_py_generic_services v then (
    Pbrt.Encoder.bool v.py_generic_services encoder;
    Pbrt.Encoder.key 18 Pbrt.Varint encoder; 
  );
  if file_options_has_php_generic_services v then (
    Pbrt.Encoder.bool v.php_generic_services encoder;
    Pbrt.Encoder.key 42 Pbrt.Varint encoder; 
  );
  if file_options_has_deprecated v then (
    Pbrt.Encoder.bool v.deprecated encoder;
    Pbrt.Encoder.key 23 Pbrt.Varint encoder; 
  );
  if file_options_has_cc_enable_arenas v then (
    Pbrt.Encoder.bool v.cc_enable_arenas encoder;
    Pbrt.Encoder.key 31 Pbrt.Varint encoder; 
  );
  if file_options_has_objc_class_prefix v then (
    Pbrt.Encoder.string v.objc_class_prefix encoder;
    Pbrt.Encoder.key 36 Pbrt.Bytes encoder; 
  );
  if file_options_has_csharp_namespace v then (
    Pbrt.Encoder.string v.csharp_namespace encoder;
    Pbrt.Encoder.key 37 Pbrt.Bytes encoder; 
  );
  if file_options_has_swift_prefix v then (
    Pbrt.Encoder.string v.swift_prefix encoder;
    Pbrt.Encoder.key 39 Pbrt.Bytes encoder; 
  );
  if file_options_has_php_class_prefix v then (
    Pbrt.Encoder.string v.php_class_prefix encoder;
    Pbrt.Encoder.key 40 Pbrt.Bytes encoder; 
  );
  if file_options_has_php_namespace v then (
    Pbrt.Encoder.string v.php_namespace encoder;
    Pbrt.Encoder.key 41 Pbrt.Bytes encoder; 
  );
  if file_options_has_php_metadata_namespace v then (
    Pbrt.Encoder.string v.php_metadata_namespace encoder;
    Pbrt.Encoder.key 44 Pbrt.Bytes encoder; 
  );
  if file_options_has_ruby_package v then (
    Pbrt.Encoder.string v.ruby_package encoder;
    Pbrt.Encoder.key 45 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_uninterpreted_option x encoder;
    Pbrt.Encoder.key 999 Pbrt.Bytes encoder; 
  ) v.uninterpreted_option encoder;
  ()

let rec encode_pb_source_code_info_location (v:source_code_info_location) encoder = 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder ->
      Pbrt.Encoder.int32_as_varint x encoder;
    ) lst encoder;
  ) v.path encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder ->
      Pbrt.Encoder.int32_as_varint x encoder;
    ) lst encoder;
  ) v.span encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  if source_code_info_location_has_leading_comments v then (
    Pbrt.Encoder.string v.leading_comments encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  if source_code_info_location_has_trailing_comments v then (
    Pbrt.Encoder.string v.trailing_comments encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.leading_detached_comments encoder;
  ()

let rec encode_pb_source_code_info (v:source_code_info) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_source_code_info_location x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.location encoder;
  ()

let rec encode_pb_file_descriptor_proto (v:file_descriptor_proto) encoder = 
  if file_descriptor_proto_has_name v then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if file_descriptor_proto_has_package v then (
    Pbrt.Encoder.string v.package encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.dependency encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.int32_as_varint x encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  ) v.public_dependency encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.int32_as_varint x encoder;
    Pbrt.Encoder.key 11 Pbrt.Varint encoder; 
  ) v.weak_dependency encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_descriptor_proto x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.message_type encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_enum_descriptor_proto x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.enum_type encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_service_descriptor_proto x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.service encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_field_descriptor_proto x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ) v.extension encoder;
  begin match v.options with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_file_options x encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  begin match v.source_code_info with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_source_code_info x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  if file_descriptor_proto_has_syntax v then (
    Pbrt.Encoder.string v.syntax encoder;
    Pbrt.Encoder.key 12 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_file_descriptor_set (v:file_descriptor_set) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_file_descriptor_proto x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.file encoder;
  ()

let rec encode_pb_generated_code_info_annotation (v:generated_code_info_annotation) encoder = 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder ->
      Pbrt.Encoder.int32_as_varint x encoder;
    ) lst encoder;
  ) v.path encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  if generated_code_info_annotation_has_source_file v then (
    Pbrt.Encoder.string v.source_file encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if generated_code_info_annotation_has_begin_ v then (
    Pbrt.Encoder.int32_as_varint v.begin_ encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  if generated_code_info_annotation_has_end_ v then (
    Pbrt.Encoder.int32_as_varint v.end_ encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_generated_code_info (v:generated_code_info) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_generated_code_info_annotation x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.annotation encoder;
  ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_field_descriptor_proto_label d : field_descriptor_proto_label = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> Label_optional
  | 2 -> Label_required
  | 3 -> Label_repeated
  | _ -> Pbrt.Decoder.malformed_variant "field_descriptor_proto_label"

let rec decode_pb_field_descriptor_proto_type d : field_descriptor_proto_type = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> Type_double
  | 2 -> Type_float
  | 3 -> Type_int64
  | 4 -> Type_uint64
  | 5 -> Type_int32
  | 6 -> Type_fixed64
  | 7 -> Type_fixed32
  | 8 -> Type_bool
  | 9 -> Type_string
  | 10 -> Type_group
  | 11 -> Type_message
  | 12 -> Type_bytes
  | 13 -> Type_uint32
  | 14 -> Type_enum
  | 15 -> Type_sfixed32
  | 16 -> Type_sfixed64
  | 17 -> Type_sint32
  | 18 -> Type_sint64
  | _ -> Pbrt.Decoder.malformed_variant "field_descriptor_proto_type"

let rec decode_pb_field_options_ctype d : field_options_ctype = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> String
  | 1 -> Cord
  | 2 -> String_piece
  | _ -> Pbrt.Decoder.malformed_variant "field_options_ctype"

let rec decode_pb_field_options_jstype d : field_options_jstype = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> Js_normal
  | 1 -> Js_string
  | 2 -> Js_number
  | _ -> Pbrt.Decoder.malformed_variant "field_options_jstype"

let rec decode_pb_uninterpreted_option_name_part d =
  let v = default_uninterpreted_option_name_part () in
  let continue__= ref true in
  let is_extension_is_set = ref false in
  let name_part_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      uninterpreted_option_name_part_set_name_part v (Pbrt.Decoder.string d); name_part_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option_name_part" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      uninterpreted_option_name_part_set_is_extension v (Pbrt.Decoder.bool d); is_extension_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option_name_part" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !is_extension_is_set then Pbrt.Decoder.missing_field "is_extension" end;
  begin if not !name_part_is_set then Pbrt.Decoder.missing_field "name_part" end;
  (v : uninterpreted_option_name_part)

let rec decode_pb_uninterpreted_option d =
  let v = default_uninterpreted_option () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      uninterpreted_option_set_name v (List.rev v.name);
    ); continue__ := false
    | Some (2, Pbrt.Bytes) -> begin
      uninterpreted_option_set_name v ((decode_pb_uninterpreted_option_name_part (Pbrt.Decoder.nested d)) :: v.name);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      uninterpreted_option_set_identifier_value v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option" 3 pk
    | Some (4, Pbrt.Varint) -> begin
      uninterpreted_option_set_positive_int_value v (Pbrt.Decoder.int64_as_varint d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option" 4 pk
    | Some (5, Pbrt.Varint) -> begin
      uninterpreted_option_set_negative_int_value v (Pbrt.Decoder.int64_as_varint d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option" 5 pk
    | Some (6, Pbrt.Bits64) -> begin
      uninterpreted_option_set_double_value v (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option" 6 pk
    | Some (7, Pbrt.Bytes) -> begin
      uninterpreted_option_set_string_value v (Pbrt.Decoder.bytes d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option" 7 pk
    | Some (8, Pbrt.Bytes) -> begin
      uninterpreted_option_set_aggregate_value v (Pbrt.Decoder.string d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "uninterpreted_option" 8 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : uninterpreted_option)

let rec decode_pb_field_options d =
  let v = default_field_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      field_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      field_options_set_ctype v (decode_pb_field_options_ctype d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      field_options_set_packed v (Pbrt.Decoder.bool d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 2 pk
    | Some (6, Pbrt.Varint) -> begin
      field_options_set_jstype v (decode_pb_field_options_jstype d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 6 pk
    | Some (5, Pbrt.Varint) -> begin
      field_options_set_lazy_ v (Pbrt.Decoder.bool d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 5 pk
    | Some (15, Pbrt.Varint) -> begin
      field_options_set_unverified_lazy v (Pbrt.Decoder.bool d);
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 15 pk
    | Some (3, Pbrt.Varint) -> begin
      field_options_set_deprecated v (Pbrt.Decoder.bool d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 3 pk
    | Some (10, Pbrt.Varint) -> begin
      field_options_set_weak v (Pbrt.Decoder.bool d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 10 pk
    | Some (999, Pbrt.Bytes) -> begin
      field_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : field_options)

let rec decode_pb_field_descriptor_proto d =
  let v = default_field_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      field_descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 1 pk
    | Some (3, Pbrt.Varint) -> begin
      field_descriptor_proto_set_number v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 3 pk
    | Some (4, Pbrt.Varint) -> begin
      field_descriptor_proto_set_label v (decode_pb_field_descriptor_proto_label d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 4 pk
    | Some (5, Pbrt.Varint) -> begin
      field_descriptor_proto_set_type_ v (decode_pb_field_descriptor_proto_type d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 5 pk
    | Some (6, Pbrt.Bytes) -> begin
      field_descriptor_proto_set_type_name v (Pbrt.Decoder.string d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 6 pk
    | Some (2, Pbrt.Bytes) -> begin
      field_descriptor_proto_set_extendee v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 2 pk
    | Some (7, Pbrt.Bytes) -> begin
      field_descriptor_proto_set_default_value v (Pbrt.Decoder.string d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 7 pk
    | Some (9, Pbrt.Varint) -> begin
      field_descriptor_proto_set_oneof_index v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 9 pk
    | Some (10, Pbrt.Bytes) -> begin
      field_descriptor_proto_set_json_name v (Pbrt.Decoder.string d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 10 pk
    | Some (8, Pbrt.Bytes) -> begin
      field_descriptor_proto_set_options v (decode_pb_field_options (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 8 pk
    | Some (17, Pbrt.Varint) -> begin
      field_descriptor_proto_set_proto3_optional v (Pbrt.Decoder.bool d);
    end
    | Some (17, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "field_descriptor_proto" 17 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : field_descriptor_proto)

let rec decode_pb_enum_value_options d =
  let v = default_enum_value_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      enum_value_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      enum_value_options_set_deprecated v (Pbrt.Decoder.bool d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_value_options" 1 pk
    | Some (999, Pbrt.Bytes) -> begin
      enum_value_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_value_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : enum_value_options)

let rec decode_pb_enum_value_descriptor_proto d =
  let v = default_enum_value_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      enum_value_descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_value_descriptor_proto" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      enum_value_descriptor_proto_set_number v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_value_descriptor_proto" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      enum_value_descriptor_proto_set_options v (decode_pb_enum_value_options (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_value_descriptor_proto" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : enum_value_descriptor_proto)

let rec decode_pb_enum_options d =
  let v = default_enum_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      enum_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (2, Pbrt.Varint) -> begin
      enum_options_set_allow_alias v (Pbrt.Decoder.bool d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_options" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      enum_options_set_deprecated v (Pbrt.Decoder.bool d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_options" 3 pk
    | Some (999, Pbrt.Bytes) -> begin
      enum_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : enum_options)

let rec decode_pb_enum_descriptor_proto_enum_reserved_range d =
  let v = default_enum_descriptor_proto_enum_reserved_range () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      enum_descriptor_proto_enum_reserved_range_set_start v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_descriptor_proto_enum_reserved_range" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      enum_descriptor_proto_enum_reserved_range_set_end_ v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_descriptor_proto_enum_reserved_range" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : enum_descriptor_proto_enum_reserved_range)

let rec decode_pb_enum_descriptor_proto d =
  let v = default_enum_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      enum_descriptor_proto_set_reserved_name v (List.rev v.reserved_name);
      enum_descriptor_proto_set_reserved_range v (List.rev v.reserved_range);
      enum_descriptor_proto_set_value v (List.rev v.value);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      enum_descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_descriptor_proto" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      enum_descriptor_proto_set_value v ((decode_pb_enum_value_descriptor_proto (Pbrt.Decoder.nested d)) :: v.value);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_descriptor_proto" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      enum_descriptor_proto_set_options v (decode_pb_enum_options (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_descriptor_proto" 3 pk
    | Some (4, Pbrt.Bytes) -> begin
      enum_descriptor_proto_set_reserved_range v ((decode_pb_enum_descriptor_proto_enum_reserved_range (Pbrt.Decoder.nested d)) :: v.reserved_range);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_descriptor_proto" 4 pk
    | Some (5, Pbrt.Bytes) -> begin
      enum_descriptor_proto_set_reserved_name v ((Pbrt.Decoder.string d) :: v.reserved_name);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "enum_descriptor_proto" 5 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : enum_descriptor_proto)

let rec decode_pb_extension_range_options d =
  let v = default_extension_range_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      extension_range_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (999, Pbrt.Bytes) -> begin
      extension_range_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "extension_range_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : extension_range_options)

let rec decode_pb_descriptor_proto_extension_range d =
  let v = default_descriptor_proto_extension_range () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      descriptor_proto_extension_range_set_start v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto_extension_range" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      descriptor_proto_extension_range_set_end_ v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto_extension_range" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      descriptor_proto_extension_range_set_options v (decode_pb_extension_range_options (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto_extension_range" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : descriptor_proto_extension_range)

let rec decode_pb_oneof_options d =
  let v = default_oneof_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      oneof_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (999, Pbrt.Bytes) -> begin
      oneof_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "oneof_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : oneof_options)

let rec decode_pb_oneof_descriptor_proto d =
  let v = default_oneof_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      oneof_descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "oneof_descriptor_proto" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      oneof_descriptor_proto_set_options v (decode_pb_oneof_options (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "oneof_descriptor_proto" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : oneof_descriptor_proto)

let rec decode_pb_message_options d =
  let v = default_message_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      message_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      message_options_set_message_set_wire_format v (Pbrt.Decoder.bool d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "message_options" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      message_options_set_no_standard_descriptor_accessor v (Pbrt.Decoder.bool d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "message_options" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      message_options_set_deprecated v (Pbrt.Decoder.bool d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "message_options" 3 pk
    | Some (7, Pbrt.Varint) -> begin
      message_options_set_map_entry v (Pbrt.Decoder.bool d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "message_options" 7 pk
    | Some (999, Pbrt.Bytes) -> begin
      message_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "message_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : message_options)

let rec decode_pb_descriptor_proto_reserved_range d =
  let v = default_descriptor_proto_reserved_range () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      descriptor_proto_reserved_range_set_start v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto_reserved_range" 1 pk
    | Some (2, Pbrt.Varint) -> begin
      descriptor_proto_reserved_range_set_end_ v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto_reserved_range" 2 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : descriptor_proto_reserved_range)

let rec decode_pb_descriptor_proto d =
  let v = default_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      descriptor_proto_set_reserved_name v (List.rev v.reserved_name);
      descriptor_proto_set_reserved_range v (List.rev v.reserved_range);
      descriptor_proto_set_oneof_decl v (List.rev v.oneof_decl);
      descriptor_proto_set_extension_range v (List.rev v.extension_range);
      descriptor_proto_set_enum_type v (List.rev v.enum_type);
      descriptor_proto_set_nested_type v (List.rev v.nested_type);
      descriptor_proto_set_extension v (List.rev v.extension);
      descriptor_proto_set_field v (List.rev v.field);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      descriptor_proto_set_field v ((decode_pb_field_descriptor_proto (Pbrt.Decoder.nested d)) :: v.field);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 2 pk
    | Some (6, Pbrt.Bytes) -> begin
      descriptor_proto_set_extension v ((decode_pb_field_descriptor_proto (Pbrt.Decoder.nested d)) :: v.extension);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 6 pk
    | Some (3, Pbrt.Bytes) -> begin
      descriptor_proto_set_nested_type v ((decode_pb_descriptor_proto (Pbrt.Decoder.nested d)) :: v.nested_type);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 3 pk
    | Some (4, Pbrt.Bytes) -> begin
      descriptor_proto_set_enum_type v ((decode_pb_enum_descriptor_proto (Pbrt.Decoder.nested d)) :: v.enum_type);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 4 pk
    | Some (5, Pbrt.Bytes) -> begin
      descriptor_proto_set_extension_range v ((decode_pb_descriptor_proto_extension_range (Pbrt.Decoder.nested d)) :: v.extension_range);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 5 pk
    | Some (8, Pbrt.Bytes) -> begin
      descriptor_proto_set_oneof_decl v ((decode_pb_oneof_descriptor_proto (Pbrt.Decoder.nested d)) :: v.oneof_decl);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 8 pk
    | Some (7, Pbrt.Bytes) -> begin
      descriptor_proto_set_options v (decode_pb_message_options (Pbrt.Decoder.nested d));
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 7 pk
    | Some (9, Pbrt.Bytes) -> begin
      descriptor_proto_set_reserved_range v ((decode_pb_descriptor_proto_reserved_range (Pbrt.Decoder.nested d)) :: v.reserved_range);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 9 pk
    | Some (10, Pbrt.Bytes) -> begin
      descriptor_proto_set_reserved_name v ((Pbrt.Decoder.string d) :: v.reserved_name);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "descriptor_proto" 10 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : descriptor_proto)

let rec decode_pb_method_options_idempotency_level d : method_options_idempotency_level = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> Idempotency_unknown
  | 1 -> No_side_effects
  | 2 -> Idempotent
  | _ -> Pbrt.Decoder.malformed_variant "method_options_idempotency_level"

let rec decode_pb_method_options d =
  let v = default_method_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      method_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (33, Pbrt.Varint) -> begin
      method_options_set_deprecated v (Pbrt.Decoder.bool d);
    end
    | Some (33, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_options" 33 pk
    | Some (34, Pbrt.Varint) -> begin
      method_options_set_idempotency_level v (decode_pb_method_options_idempotency_level d);
    end
    | Some (34, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_options" 34 pk
    | Some (999, Pbrt.Bytes) -> begin
      method_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : method_options)

let rec decode_pb_method_descriptor_proto d =
  let v = default_method_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      method_descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_descriptor_proto" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      method_descriptor_proto_set_input_type v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_descriptor_proto" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      method_descriptor_proto_set_output_type v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_descriptor_proto" 3 pk
    | Some (4, Pbrt.Bytes) -> begin
      method_descriptor_proto_set_options v (decode_pb_method_options (Pbrt.Decoder.nested d));
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_descriptor_proto" 4 pk
    | Some (5, Pbrt.Varint) -> begin
      method_descriptor_proto_set_client_streaming v (Pbrt.Decoder.bool d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_descriptor_proto" 5 pk
    | Some (6, Pbrt.Varint) -> begin
      method_descriptor_proto_set_server_streaming v (Pbrt.Decoder.bool d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "method_descriptor_proto" 6 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : method_descriptor_proto)

let rec decode_pb_service_options d =
  let v = default_service_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      service_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (33, Pbrt.Varint) -> begin
      service_options_set_deprecated v (Pbrt.Decoder.bool d);
    end
    | Some (33, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "service_options" 33 pk
    | Some (999, Pbrt.Bytes) -> begin
      service_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "service_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : service_options)

let rec decode_pb_service_descriptor_proto d =
  let v = default_service_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      service_descriptor_proto_set_method_ v (List.rev v.method_);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      service_descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "service_descriptor_proto" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      service_descriptor_proto_set_method_ v ((decode_pb_method_descriptor_proto (Pbrt.Decoder.nested d)) :: v.method_);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "service_descriptor_proto" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      service_descriptor_proto_set_options v (decode_pb_service_options (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "service_descriptor_proto" 3 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : service_descriptor_proto)

let rec decode_pb_file_options_optimize_mode d : file_options_optimize_mode = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> Speed
  | 2 -> Code_size
  | 3 -> Lite_runtime
  | _ -> Pbrt.Decoder.malformed_variant "file_options_optimize_mode"

let rec decode_pb_file_options d =
  let v = default_file_options () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      file_options_set_uninterpreted_option v (List.rev v.uninterpreted_option);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      file_options_set_java_package v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 1 pk
    | Some (8, Pbrt.Bytes) -> begin
      file_options_set_java_outer_classname v (Pbrt.Decoder.string d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 8 pk
    | Some (10, Pbrt.Varint) -> begin
      file_options_set_java_multiple_files v (Pbrt.Decoder.bool d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 10 pk
    | Some (20, Pbrt.Varint) -> begin
      file_options_set_java_generate_equals_and_hash v (Pbrt.Decoder.bool d);
    end
    | Some (20, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 20 pk
    | Some (27, Pbrt.Varint) -> begin
      file_options_set_java_string_check_utf8 v (Pbrt.Decoder.bool d);
    end
    | Some (27, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 27 pk
    | Some (9, Pbrt.Varint) -> begin
      file_options_set_optimize_for v (decode_pb_file_options_optimize_mode d);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 9 pk
    | Some (11, Pbrt.Bytes) -> begin
      file_options_set_go_package v (Pbrt.Decoder.string d);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 11 pk
    | Some (16, Pbrt.Varint) -> begin
      file_options_set_cc_generic_services v (Pbrt.Decoder.bool d);
    end
    | Some (16, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 16 pk
    | Some (17, Pbrt.Varint) -> begin
      file_options_set_java_generic_services v (Pbrt.Decoder.bool d);
    end
    | Some (17, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 17 pk
    | Some (18, Pbrt.Varint) -> begin
      file_options_set_py_generic_services v (Pbrt.Decoder.bool d);
    end
    | Some (18, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 18 pk
    | Some (42, Pbrt.Varint) -> begin
      file_options_set_php_generic_services v (Pbrt.Decoder.bool d);
    end
    | Some (42, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 42 pk
    | Some (23, Pbrt.Varint) -> begin
      file_options_set_deprecated v (Pbrt.Decoder.bool d);
    end
    | Some (23, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 23 pk
    | Some (31, Pbrt.Varint) -> begin
      file_options_set_cc_enable_arenas v (Pbrt.Decoder.bool d);
    end
    | Some (31, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 31 pk
    | Some (36, Pbrt.Bytes) -> begin
      file_options_set_objc_class_prefix v (Pbrt.Decoder.string d);
    end
    | Some (36, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 36 pk
    | Some (37, Pbrt.Bytes) -> begin
      file_options_set_csharp_namespace v (Pbrt.Decoder.string d);
    end
    | Some (37, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 37 pk
    | Some (39, Pbrt.Bytes) -> begin
      file_options_set_swift_prefix v (Pbrt.Decoder.string d);
    end
    | Some (39, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 39 pk
    | Some (40, Pbrt.Bytes) -> begin
      file_options_set_php_class_prefix v (Pbrt.Decoder.string d);
    end
    | Some (40, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 40 pk
    | Some (41, Pbrt.Bytes) -> begin
      file_options_set_php_namespace v (Pbrt.Decoder.string d);
    end
    | Some (41, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 41 pk
    | Some (44, Pbrt.Bytes) -> begin
      file_options_set_php_metadata_namespace v (Pbrt.Decoder.string d);
    end
    | Some (44, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 44 pk
    | Some (45, Pbrt.Bytes) -> begin
      file_options_set_ruby_package v (Pbrt.Decoder.string d);
    end
    | Some (45, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 45 pk
    | Some (999, Pbrt.Bytes) -> begin
      file_options_set_uninterpreted_option v ((decode_pb_uninterpreted_option (Pbrt.Decoder.nested d)) :: v.uninterpreted_option);
    end
    | Some (999, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_options" 999 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : file_options)

let rec decode_pb_source_code_info_location d =
  let v = default_source_code_info_location () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      source_code_info_location_set_leading_detached_comments v (List.rev v.leading_detached_comments);
      source_code_info_location_set_span v (List.rev v.span);
      source_code_info_location_set_path v (List.rev v.path);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      source_code_info_location_set_path v @@ Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int32_as_varint d)::l) [] d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "source_code_info_location" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      source_code_info_location_set_span v @@ Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int32_as_varint d)::l) [] d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "source_code_info_location" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      source_code_info_location_set_leading_comments v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "source_code_info_location" 3 pk
    | Some (4, Pbrt.Bytes) -> begin
      source_code_info_location_set_trailing_comments v (Pbrt.Decoder.string d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "source_code_info_location" 4 pk
    | Some (6, Pbrt.Bytes) -> begin
      source_code_info_location_set_leading_detached_comments v ((Pbrt.Decoder.string d) :: v.leading_detached_comments);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "source_code_info_location" 6 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : source_code_info_location)

let rec decode_pb_source_code_info d =
  let v = default_source_code_info () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      source_code_info_set_location v (List.rev v.location);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      source_code_info_set_location v ((decode_pb_source_code_info_location (Pbrt.Decoder.nested d)) :: v.location);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "source_code_info" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : source_code_info)

let rec decode_pb_file_descriptor_proto d =
  let v = default_file_descriptor_proto () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      file_descriptor_proto_set_extension v (List.rev v.extension);
      file_descriptor_proto_set_service v (List.rev v.service);
      file_descriptor_proto_set_enum_type v (List.rev v.enum_type);
      file_descriptor_proto_set_message_type v (List.rev v.message_type);
      file_descriptor_proto_set_weak_dependency v (List.rev v.weak_dependency);
      file_descriptor_proto_set_public_dependency v (List.rev v.public_dependency);
      file_descriptor_proto_set_dependency v (List.rev v.dependency);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_package v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 2 pk
    | Some (3, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_dependency v ((Pbrt.Decoder.string d) :: v.dependency);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 3 pk
    | Some (10, Pbrt.Varint) -> begin
      file_descriptor_proto_set_public_dependency v ((Pbrt.Decoder.int32_as_varint d) :: v.public_dependency);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 10 pk
    | Some (11, Pbrt.Varint) -> begin
      file_descriptor_proto_set_weak_dependency v ((Pbrt.Decoder.int32_as_varint d) :: v.weak_dependency);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 11 pk
    | Some (4, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_message_type v ((decode_pb_descriptor_proto (Pbrt.Decoder.nested d)) :: v.message_type);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 4 pk
    | Some (5, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_enum_type v ((decode_pb_enum_descriptor_proto (Pbrt.Decoder.nested d)) :: v.enum_type);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 5 pk
    | Some (6, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_service v ((decode_pb_service_descriptor_proto (Pbrt.Decoder.nested d)) :: v.service);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 6 pk
    | Some (7, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_extension v ((decode_pb_field_descriptor_proto (Pbrt.Decoder.nested d)) :: v.extension);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 7 pk
    | Some (8, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_options v (decode_pb_file_options (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 8 pk
    | Some (9, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_source_code_info v (decode_pb_source_code_info (Pbrt.Decoder.nested d));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 9 pk
    | Some (12, Pbrt.Bytes) -> begin
      file_descriptor_proto_set_syntax v (Pbrt.Decoder.string d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_proto" 12 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : file_descriptor_proto)

let rec decode_pb_file_descriptor_set d =
  let v = default_file_descriptor_set () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      file_descriptor_set_set_file v (List.rev v.file);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      file_descriptor_set_set_file v ((decode_pb_file_descriptor_proto (Pbrt.Decoder.nested d)) :: v.file);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "file_descriptor_set" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : file_descriptor_set)

let rec decode_pb_generated_code_info_annotation d =
  let v = default_generated_code_info_annotation () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      generated_code_info_annotation_set_path v (List.rev v.path);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      generated_code_info_annotation_set_path v @@ Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int32_as_varint d)::l) [] d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "generated_code_info_annotation" 1 pk
    | Some (2, Pbrt.Bytes) -> begin
      generated_code_info_annotation_set_source_file v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "generated_code_info_annotation" 2 pk
    | Some (3, Pbrt.Varint) -> begin
      generated_code_info_annotation_set_begin_ v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "generated_code_info_annotation" 3 pk
    | Some (4, Pbrt.Varint) -> begin
      generated_code_info_annotation_set_end_ v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "generated_code_info_annotation" 4 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : generated_code_info_annotation)

let rec decode_pb_generated_code_info d =
  let v = default_generated_code_info () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      generated_code_info_set_annotation v (List.rev v.annotation);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      generated_code_info_set_annotation v ((decode_pb_generated_code_info_annotation (Pbrt.Decoder.nested d)) :: v.annotation);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload_message "generated_code_info" 1 pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : generated_code_info)

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf YoJson Encoding} *)

let rec encode_json_field_descriptor_proto_label (v:field_descriptor_proto_label) = 
  match v with
  | Label_optional -> `String "LABEL_OPTIONAL"
  | Label_required -> `String "LABEL_REQUIRED"
  | Label_repeated -> `String "LABEL_REPEATED"

let rec encode_json_field_descriptor_proto_type (v:field_descriptor_proto_type) = 
  match v with
  | Type_double -> `String "TYPE_DOUBLE"
  | Type_float -> `String "TYPE_FLOAT"
  | Type_int64 -> `String "TYPE_INT64"
  | Type_uint64 -> `String "TYPE_UINT64"
  | Type_int32 -> `String "TYPE_INT32"
  | Type_fixed64 -> `String "TYPE_FIXED64"
  | Type_fixed32 -> `String "TYPE_FIXED32"
  | Type_bool -> `String "TYPE_BOOL"
  | Type_string -> `String "TYPE_STRING"
  | Type_group -> `String "TYPE_GROUP"
  | Type_message -> `String "TYPE_MESSAGE"
  | Type_bytes -> `String "TYPE_BYTES"
  | Type_uint32 -> `String "TYPE_UINT32"
  | Type_enum -> `String "TYPE_ENUM"
  | Type_sfixed32 -> `String "TYPE_SFIXED32"
  | Type_sfixed64 -> `String "TYPE_SFIXED64"
  | Type_sint32 -> `String "TYPE_SINT32"
  | Type_sint64 -> `String "TYPE_SINT64"

let rec encode_json_field_options_ctype (v:field_options_ctype) = 
  match v with
  | String -> `String "STRING"
  | Cord -> `String "CORD"
  | String_piece -> `String "STRING_PIECE"

let rec encode_json_field_options_jstype (v:field_options_jstype) = 
  match v with
  | Js_normal -> `String "JS_NORMAL"
  | Js_string -> `String "JS_STRING"
  | Js_number -> `String "JS_NUMBER"

let rec encode_json_uninterpreted_option_name_part (v:uninterpreted_option_name_part) = 
  let assoc = ref [] in
  assoc := ("namePart", Pbrt_yojson.make_string v.name_part) :: !assoc;
  assoc := ("isExtension", Pbrt_yojson.make_bool v.is_extension) :: !assoc;
  `Assoc !assoc

let rec encode_json_uninterpreted_option (v:uninterpreted_option) = 
  let assoc = ref [] in
  assoc := (
    let l = v.name |> List.map encode_json_uninterpreted_option_name_part in
    ("name", `List l) :: !assoc 
  );
  if uninterpreted_option_has_identifier_value v then (
    assoc := ("identifierValue", Pbrt_yojson.make_string v.identifier_value) :: !assoc;
  );
  if uninterpreted_option_has_positive_int_value v then (
    assoc := ("positiveIntValue", Pbrt_yojson.make_string (Int64.to_string v.positive_int_value)) :: !assoc;
  );
  if uninterpreted_option_has_negative_int_value v then (
    assoc := ("negativeIntValue", Pbrt_yojson.make_string (Int64.to_string v.negative_int_value)) :: !assoc;
  );
  if uninterpreted_option_has_double_value v then (
    assoc := ("doubleValue", Pbrt_yojson.make_string (string_of_float v.double_value)) :: !assoc;
  );
  if uninterpreted_option_has_string_value v then (
    assoc := ("stringValue", Pbrt_yojson.make_bytes v.string_value) :: !assoc;
  );
  if uninterpreted_option_has_aggregate_value v then (
    assoc := ("aggregateValue", Pbrt_yojson.make_string v.aggregate_value) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_field_options (v:field_options) = 
  let assoc = ref [] in
  if field_options_has_ctype v then (
    assoc := ("ctype", encode_json_field_options_ctype v.ctype) :: !assoc;
  );
  if field_options_has_packed v then (
    assoc := ("packed", Pbrt_yojson.make_bool v.packed) :: !assoc;
  );
  if field_options_has_jstype v then (
    assoc := ("jstype", encode_json_field_options_jstype v.jstype) :: !assoc;
  );
  if field_options_has_lazy_ v then (
    assoc := ("lazy", Pbrt_yojson.make_bool v.lazy_) :: !assoc;
  );
  if field_options_has_unverified_lazy v then (
    assoc := ("unverifiedLazy", Pbrt_yojson.make_bool v.unverified_lazy) :: !assoc;
  );
  if field_options_has_deprecated v then (
    assoc := ("deprecated", Pbrt_yojson.make_bool v.deprecated) :: !assoc;
  );
  if field_options_has_weak v then (
    assoc := ("weak", Pbrt_yojson.make_bool v.weak) :: !assoc;
  );
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_field_descriptor_proto (v:field_descriptor_proto) = 
  let assoc = ref [] in
  if field_descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  if field_descriptor_proto_has_number v then (
    assoc := ("number", Pbrt_yojson.make_int (Int32.to_int v.number)) :: !assoc;
  );
  if field_descriptor_proto_has_label v then (
    assoc := ("label", encode_json_field_descriptor_proto_label v.label) :: !assoc;
  );
  if field_descriptor_proto_has_type_ v then (
    assoc := ("type", encode_json_field_descriptor_proto_type v.type_) :: !assoc;
  );
  if field_descriptor_proto_has_type_name v then (
    assoc := ("typeName", Pbrt_yojson.make_string v.type_name) :: !assoc;
  );
  if field_descriptor_proto_has_extendee v then (
    assoc := ("extendee", Pbrt_yojson.make_string v.extendee) :: !assoc;
  );
  if field_descriptor_proto_has_default_value v then (
    assoc := ("defaultValue", Pbrt_yojson.make_string v.default_value) :: !assoc;
  );
  if field_descriptor_proto_has_oneof_index v then (
    assoc := ("oneofIndex", Pbrt_yojson.make_int (Int32.to_int v.oneof_index)) :: !assoc;
  );
  if field_descriptor_proto_has_json_name v then (
    assoc := ("jsonName", Pbrt_yojson.make_string v.json_name) :: !assoc;
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_field_options v) :: !assoc);
  if field_descriptor_proto_has_proto3_optional v then (
    assoc := ("proto3Optional", Pbrt_yojson.make_bool v.proto3_optional) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_enum_value_options (v:enum_value_options) = 
  let assoc = ref [] in
  if enum_value_options_has_deprecated v then (
    assoc := ("deprecated", Pbrt_yojson.make_bool v.deprecated) :: !assoc;
  );
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_enum_value_descriptor_proto (v:enum_value_descriptor_proto) = 
  let assoc = ref [] in
  if enum_value_descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  if enum_value_descriptor_proto_has_number v then (
    assoc := ("number", Pbrt_yojson.make_int (Int32.to_int v.number)) :: !assoc;
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_enum_value_options v) :: !assoc);
  `Assoc !assoc

let rec encode_json_enum_options (v:enum_options) = 
  let assoc = ref [] in
  if enum_options_has_allow_alias v then (
    assoc := ("allowAlias", Pbrt_yojson.make_bool v.allow_alias) :: !assoc;
  );
  if enum_options_has_deprecated v then (
    assoc := ("deprecated", Pbrt_yojson.make_bool v.deprecated) :: !assoc;
  );
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_enum_descriptor_proto_enum_reserved_range (v:enum_descriptor_proto_enum_reserved_range) = 
  let assoc = ref [] in
  if enum_descriptor_proto_enum_reserved_range_has_start v then (
    assoc := ("start", Pbrt_yojson.make_int (Int32.to_int v.start)) :: !assoc;
  );
  if enum_descriptor_proto_enum_reserved_range_has_end_ v then (
    assoc := ("end", Pbrt_yojson.make_int (Int32.to_int v.end_)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_enum_descriptor_proto (v:enum_descriptor_proto) = 
  let assoc = ref [] in
  if enum_descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  assoc := (
    let l = v.value |> List.map encode_json_enum_value_descriptor_proto in
    ("value", `List l) :: !assoc 
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_enum_options v) :: !assoc);
  assoc := (
    let l = v.reserved_range |> List.map encode_json_enum_descriptor_proto_enum_reserved_range in
    ("reservedRange", `List l) :: !assoc 
  );
  assoc := (
    let l = v.reserved_name |> List.map Pbrt_yojson.make_string in
    ("reservedName", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_extension_range_options (v:extension_range_options) = 
  let assoc = ref [] in
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_descriptor_proto_extension_range (v:descriptor_proto_extension_range) = 
  let assoc = ref [] in
  if descriptor_proto_extension_range_has_start v then (
    assoc := ("start", Pbrt_yojson.make_int (Int32.to_int v.start)) :: !assoc;
  );
  if descriptor_proto_extension_range_has_end_ v then (
    assoc := ("end", Pbrt_yojson.make_int (Int32.to_int v.end_)) :: !assoc;
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_extension_range_options v) :: !assoc);
  `Assoc !assoc

let rec encode_json_oneof_options (v:oneof_options) = 
  let assoc = ref [] in
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_oneof_descriptor_proto (v:oneof_descriptor_proto) = 
  let assoc = ref [] in
  if oneof_descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_oneof_options v) :: !assoc);
  `Assoc !assoc

let rec encode_json_message_options (v:message_options) = 
  let assoc = ref [] in
  if message_options_has_message_set_wire_format v then (
    assoc := ("messageSetWireFormat", Pbrt_yojson.make_bool v.message_set_wire_format) :: !assoc;
  );
  if message_options_has_no_standard_descriptor_accessor v then (
    assoc := ("noStandardDescriptorAccessor", Pbrt_yojson.make_bool v.no_standard_descriptor_accessor) :: !assoc;
  );
  if message_options_has_deprecated v then (
    assoc := ("deprecated", Pbrt_yojson.make_bool v.deprecated) :: !assoc;
  );
  if message_options_has_map_entry v then (
    assoc := ("mapEntry", Pbrt_yojson.make_bool v.map_entry) :: !assoc;
  );
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_descriptor_proto_reserved_range (v:descriptor_proto_reserved_range) = 
  let assoc = ref [] in
  if descriptor_proto_reserved_range_has_start v then (
    assoc := ("start", Pbrt_yojson.make_int (Int32.to_int v.start)) :: !assoc;
  );
  if descriptor_proto_reserved_range_has_end_ v then (
    assoc := ("end", Pbrt_yojson.make_int (Int32.to_int v.end_)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_descriptor_proto (v:descriptor_proto) = 
  let assoc = ref [] in
  if descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  assoc := (
    let l = v.field |> List.map encode_json_field_descriptor_proto in
    ("field", `List l) :: !assoc 
  );
  assoc := (
    let l = v.extension |> List.map encode_json_field_descriptor_proto in
    ("extension", `List l) :: !assoc 
  );
  assoc := (
    let l = v.nested_type |> List.map encode_json_descriptor_proto in
    ("nestedType", `List l) :: !assoc 
  );
  assoc := (
    let l = v.enum_type |> List.map encode_json_enum_descriptor_proto in
    ("enumType", `List l) :: !assoc 
  );
  assoc := (
    let l = v.extension_range |> List.map encode_json_descriptor_proto_extension_range in
    ("extensionRange", `List l) :: !assoc 
  );
  assoc := (
    let l = v.oneof_decl |> List.map encode_json_oneof_descriptor_proto in
    ("oneofDecl", `List l) :: !assoc 
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_message_options v) :: !assoc);
  assoc := (
    let l = v.reserved_range |> List.map encode_json_descriptor_proto_reserved_range in
    ("reservedRange", `List l) :: !assoc 
  );
  assoc := (
    let l = v.reserved_name |> List.map Pbrt_yojson.make_string in
    ("reservedName", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_method_options_idempotency_level (v:method_options_idempotency_level) = 
  match v with
  | Idempotency_unknown -> `String "IDEMPOTENCY_UNKNOWN"
  | No_side_effects -> `String "NO_SIDE_EFFECTS"
  | Idempotent -> `String "IDEMPOTENT"

let rec encode_json_method_options (v:method_options) = 
  let assoc = ref [] in
  if method_options_has_deprecated v then (
    assoc := ("deprecated", Pbrt_yojson.make_bool v.deprecated) :: !assoc;
  );
  if method_options_has_idempotency_level v then (
    assoc := ("idempotencyLevel", encode_json_method_options_idempotency_level v.idempotency_level) :: !assoc;
  );
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_method_descriptor_proto (v:method_descriptor_proto) = 
  let assoc = ref [] in
  if method_descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  if method_descriptor_proto_has_input_type v then (
    assoc := ("inputType", Pbrt_yojson.make_string v.input_type) :: !assoc;
  );
  if method_descriptor_proto_has_output_type v then (
    assoc := ("outputType", Pbrt_yojson.make_string v.output_type) :: !assoc;
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_method_options v) :: !assoc);
  if method_descriptor_proto_has_client_streaming v then (
    assoc := ("clientStreaming", Pbrt_yojson.make_bool v.client_streaming) :: !assoc;
  );
  if method_descriptor_proto_has_server_streaming v then (
    assoc := ("serverStreaming", Pbrt_yojson.make_bool v.server_streaming) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_service_options (v:service_options) = 
  let assoc = ref [] in
  if service_options_has_deprecated v then (
    assoc := ("deprecated", Pbrt_yojson.make_bool v.deprecated) :: !assoc;
  );
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_service_descriptor_proto (v:service_descriptor_proto) = 
  let assoc = ref [] in
  if service_descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  assoc := (
    let l = v.method_ |> List.map encode_json_method_descriptor_proto in
    ("method", `List l) :: !assoc 
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_service_options v) :: !assoc);
  `Assoc !assoc

let rec encode_json_file_options_optimize_mode (v:file_options_optimize_mode) = 
  match v with
  | Speed -> `String "SPEED"
  | Code_size -> `String "CODE_SIZE"
  | Lite_runtime -> `String "LITE_RUNTIME"

let rec encode_json_file_options (v:file_options) = 
  let assoc = ref [] in
  if file_options_has_java_package v then (
    assoc := ("javaPackage", Pbrt_yojson.make_string v.java_package) :: !assoc;
  );
  if file_options_has_java_outer_classname v then (
    assoc := ("javaOuterClassname", Pbrt_yojson.make_string v.java_outer_classname) :: !assoc;
  );
  if file_options_has_java_multiple_files v then (
    assoc := ("javaMultipleFiles", Pbrt_yojson.make_bool v.java_multiple_files) :: !assoc;
  );
  if file_options_has_java_generate_equals_and_hash v then (
    assoc := ("javaGenerateEqualsAndHash", Pbrt_yojson.make_bool v.java_generate_equals_and_hash) :: !assoc;
  );
  if file_options_has_java_string_check_utf8 v then (
    assoc := ("javaStringCheckUtf8", Pbrt_yojson.make_bool v.java_string_check_utf8) :: !assoc;
  );
  if file_options_has_optimize_for v then (
    assoc := ("optimizeFor", encode_json_file_options_optimize_mode v.optimize_for) :: !assoc;
  );
  if file_options_has_go_package v then (
    assoc := ("goPackage", Pbrt_yojson.make_string v.go_package) :: !assoc;
  );
  if file_options_has_cc_generic_services v then (
    assoc := ("ccGenericServices", Pbrt_yojson.make_bool v.cc_generic_services) :: !assoc;
  );
  if file_options_has_java_generic_services v then (
    assoc := ("javaGenericServices", Pbrt_yojson.make_bool v.java_generic_services) :: !assoc;
  );
  if file_options_has_py_generic_services v then (
    assoc := ("pyGenericServices", Pbrt_yojson.make_bool v.py_generic_services) :: !assoc;
  );
  if file_options_has_php_generic_services v then (
    assoc := ("phpGenericServices", Pbrt_yojson.make_bool v.php_generic_services) :: !assoc;
  );
  if file_options_has_deprecated v then (
    assoc := ("deprecated", Pbrt_yojson.make_bool v.deprecated) :: !assoc;
  );
  if file_options_has_cc_enable_arenas v then (
    assoc := ("ccEnableArenas", Pbrt_yojson.make_bool v.cc_enable_arenas) :: !assoc;
  );
  if file_options_has_objc_class_prefix v then (
    assoc := ("objcClassPrefix", Pbrt_yojson.make_string v.objc_class_prefix) :: !assoc;
  );
  if file_options_has_csharp_namespace v then (
    assoc := ("csharpNamespace", Pbrt_yojson.make_string v.csharp_namespace) :: !assoc;
  );
  if file_options_has_swift_prefix v then (
    assoc := ("swiftPrefix", Pbrt_yojson.make_string v.swift_prefix) :: !assoc;
  );
  if file_options_has_php_class_prefix v then (
    assoc := ("phpClassPrefix", Pbrt_yojson.make_string v.php_class_prefix) :: !assoc;
  );
  if file_options_has_php_namespace v then (
    assoc := ("phpNamespace", Pbrt_yojson.make_string v.php_namespace) :: !assoc;
  );
  if file_options_has_php_metadata_namespace v then (
    assoc := ("phpMetadataNamespace", Pbrt_yojson.make_string v.php_metadata_namespace) :: !assoc;
  );
  if file_options_has_ruby_package v then (
    assoc := ("rubyPackage", Pbrt_yojson.make_string v.ruby_package) :: !assoc;
  );
  assoc := (
    let l = v.uninterpreted_option |> List.map encode_json_uninterpreted_option in
    ("uninterpretedOption", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_source_code_info_location (v:source_code_info_location) = 
  let assoc = ref [] in
  assoc := (
    let l = v.path |> List.map Int32.to_int |> List.map Pbrt_yojson.make_int in 
    ("path", `List l) :: !assoc 
  );
  assoc := (
    let l = v.span |> List.map Int32.to_int |> List.map Pbrt_yojson.make_int in 
    ("span", `List l) :: !assoc 
  );
  if source_code_info_location_has_leading_comments v then (
    assoc := ("leadingComments", Pbrt_yojson.make_string v.leading_comments) :: !assoc;
  );
  if source_code_info_location_has_trailing_comments v then (
    assoc := ("trailingComments", Pbrt_yojson.make_string v.trailing_comments) :: !assoc;
  );
  assoc := (
    let l = v.leading_detached_comments |> List.map Pbrt_yojson.make_string in
    ("leadingDetachedComments", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_source_code_info (v:source_code_info) = 
  let assoc = ref [] in
  assoc := (
    let l = v.location |> List.map encode_json_source_code_info_location in
    ("location", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_file_descriptor_proto (v:file_descriptor_proto) = 
  let assoc = ref [] in
  if file_descriptor_proto_has_name v then (
    assoc := ("name", Pbrt_yojson.make_string v.name) :: !assoc;
  );
  if file_descriptor_proto_has_package v then (
    assoc := ("package", Pbrt_yojson.make_string v.package) :: !assoc;
  );
  assoc := (
    let l = v.dependency |> List.map Pbrt_yojson.make_string in
    ("dependency", `List l) :: !assoc 
  );
  assoc := (
    let l = v.public_dependency |> List.map Int32.to_int |> List.map Pbrt_yojson.make_int in 
    ("publicDependency", `List l) :: !assoc 
  );
  assoc := (
    let l = v.weak_dependency |> List.map Int32.to_int |> List.map Pbrt_yojson.make_int in 
    ("weakDependency", `List l) :: !assoc 
  );
  assoc := (
    let l = v.message_type |> List.map encode_json_descriptor_proto in
    ("messageType", `List l) :: !assoc 
  );
  assoc := (
    let l = v.enum_type |> List.map encode_json_enum_descriptor_proto in
    ("enumType", `List l) :: !assoc 
  );
  assoc := (
    let l = v.service |> List.map encode_json_service_descriptor_proto in
    ("service", `List l) :: !assoc 
  );
  assoc := (
    let l = v.extension |> List.map encode_json_field_descriptor_proto in
    ("extension", `List l) :: !assoc 
  );
  assoc := (match v.options with
    | None -> !assoc
    | Some v -> ("options", encode_json_file_options v) :: !assoc);
  assoc := (match v.source_code_info with
    | None -> !assoc
    | Some v -> ("sourceCodeInfo", encode_json_source_code_info v) :: !assoc);
  if file_descriptor_proto_has_syntax v then (
    assoc := ("syntax", Pbrt_yojson.make_string v.syntax) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_file_descriptor_set (v:file_descriptor_set) = 
  let assoc = ref [] in
  assoc := (
    let l = v.file |> List.map encode_json_file_descriptor_proto in
    ("file", `List l) :: !assoc 
  );
  `Assoc !assoc

let rec encode_json_generated_code_info_annotation (v:generated_code_info_annotation) = 
  let assoc = ref [] in
  assoc := (
    let l = v.path |> List.map Int32.to_int |> List.map Pbrt_yojson.make_int in 
    ("path", `List l) :: !assoc 
  );
  if generated_code_info_annotation_has_source_file v then (
    assoc := ("sourceFile", Pbrt_yojson.make_string v.source_file) :: !assoc;
  );
  if generated_code_info_annotation_has_begin_ v then (
    assoc := ("begin", Pbrt_yojson.make_int (Int32.to_int v.begin_)) :: !assoc;
  );
  if generated_code_info_annotation_has_end_ v then (
    assoc := ("end", Pbrt_yojson.make_int (Int32.to_int v.end_)) :: !assoc;
  );
  `Assoc !assoc

let rec encode_json_generated_code_info (v:generated_code_info) = 
  let assoc = ref [] in
  assoc := (
    let l = v.annotation |> List.map encode_json_generated_code_info_annotation in
    ("annotation", `List l) :: !assoc 
  );
  `Assoc !assoc

[@@@ocaml.warning "-23-27-30-39"]

(** {2 JSON Decoding} *)

let rec decode_json_field_descriptor_proto_label json =
  match json with
  | `String "LABEL_OPTIONAL" -> (Label_optional : field_descriptor_proto_label)
  | `String "LABEL_REQUIRED" -> (Label_required : field_descriptor_proto_label)
  | `String "LABEL_REPEATED" -> (Label_repeated : field_descriptor_proto_label)
  | _ -> Pbrt_yojson.E.malformed_variant "field_descriptor_proto_label"

let rec decode_json_field_descriptor_proto_type json =
  match json with
  | `String "TYPE_DOUBLE" -> (Type_double : field_descriptor_proto_type)
  | `String "TYPE_FLOAT" -> (Type_float : field_descriptor_proto_type)
  | `String "TYPE_INT64" -> (Type_int64 : field_descriptor_proto_type)
  | `String "TYPE_UINT64" -> (Type_uint64 : field_descriptor_proto_type)
  | `String "TYPE_INT32" -> (Type_int32 : field_descriptor_proto_type)
  | `String "TYPE_FIXED64" -> (Type_fixed64 : field_descriptor_proto_type)
  | `String "TYPE_FIXED32" -> (Type_fixed32 : field_descriptor_proto_type)
  | `String "TYPE_BOOL" -> (Type_bool : field_descriptor_proto_type)
  | `String "TYPE_STRING" -> (Type_string : field_descriptor_proto_type)
  | `String "TYPE_GROUP" -> (Type_group : field_descriptor_proto_type)
  | `String "TYPE_MESSAGE" -> (Type_message : field_descriptor_proto_type)
  | `String "TYPE_BYTES" -> (Type_bytes : field_descriptor_proto_type)
  | `String "TYPE_UINT32" -> (Type_uint32 : field_descriptor_proto_type)
  | `String "TYPE_ENUM" -> (Type_enum : field_descriptor_proto_type)
  | `String "TYPE_SFIXED32" -> (Type_sfixed32 : field_descriptor_proto_type)
  | `String "TYPE_SFIXED64" -> (Type_sfixed64 : field_descriptor_proto_type)
  | `String "TYPE_SINT32" -> (Type_sint32 : field_descriptor_proto_type)
  | `String "TYPE_SINT64" -> (Type_sint64 : field_descriptor_proto_type)
  | _ -> Pbrt_yojson.E.malformed_variant "field_descriptor_proto_type"

let rec decode_json_field_options_ctype json =
  match json with
  | `String "STRING" -> (String : field_options_ctype)
  | `String "CORD" -> (Cord : field_options_ctype)
  | `String "STRING_PIECE" -> (String_piece : field_options_ctype)
  | _ -> Pbrt_yojson.E.malformed_variant "field_options_ctype"

let rec decode_json_field_options_jstype json =
  match json with
  | `String "JS_NORMAL" -> (Js_normal : field_options_jstype)
  | `String "JS_STRING" -> (Js_string : field_options_jstype)
  | `String "JS_NUMBER" -> (Js_number : field_options_jstype)
  | _ -> Pbrt_yojson.E.malformed_variant "field_options_jstype"

let rec decode_json_uninterpreted_option_name_part d =
  let v = default_uninterpreted_option_name_part () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("namePart", json_value) -> 
      uninterpreted_option_name_part_set_name_part v (Pbrt_yojson.string json_value "uninterpreted_option_name_part" "name_part")
    | ("isExtension", json_value) -> 
      uninterpreted_option_name_part_set_is_extension v (Pbrt_yojson.bool json_value "uninterpreted_option_name_part" "is_extension")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    name_part = v.name_part;
    is_extension = v.is_extension;
  } : uninterpreted_option_name_part)

let rec decode_json_uninterpreted_option d =
  let v = default_uninterpreted_option () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", `List l) -> begin
      uninterpreted_option_set_name v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option_name_part json_value)
      ) l;
    end
    | ("identifierValue", json_value) -> 
      uninterpreted_option_set_identifier_value v (Pbrt_yojson.string json_value "uninterpreted_option" "identifier_value")
    | ("positiveIntValue", json_value) -> 
      uninterpreted_option_set_positive_int_value v (Pbrt_yojson.int64 json_value "uninterpreted_option" "positive_int_value")
    | ("negativeIntValue", json_value) -> 
      uninterpreted_option_set_negative_int_value v (Pbrt_yojson.int64 json_value "uninterpreted_option" "negative_int_value")
    | ("doubleValue", json_value) -> 
      uninterpreted_option_set_double_value v (Pbrt_yojson.float json_value "uninterpreted_option" "double_value")
    | ("stringValue", json_value) -> 
      uninterpreted_option_set_string_value v (Pbrt_yojson.bytes json_value "uninterpreted_option" "string_value")
    | ("aggregateValue", json_value) -> 
      uninterpreted_option_set_aggregate_value v (Pbrt_yojson.string json_value "uninterpreted_option" "aggregate_value")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    identifier_value = v.identifier_value;
    positive_int_value = v.positive_int_value;
    negative_int_value = v.negative_int_value;
    double_value = v.double_value;
    string_value = v.string_value;
    aggregate_value = v.aggregate_value;
  } : uninterpreted_option)

let rec decode_json_field_options d =
  let v = default_field_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("ctype", json_value) -> 
      field_options_set_ctype v ((decode_json_field_options_ctype json_value))
    | ("packed", json_value) -> 
      field_options_set_packed v (Pbrt_yojson.bool json_value "field_options" "packed")
    | ("jstype", json_value) -> 
      field_options_set_jstype v ((decode_json_field_options_jstype json_value))
    | ("lazy", json_value) -> 
      field_options_set_lazy_ v (Pbrt_yojson.bool json_value "field_options" "lazy_")
    | ("unverifiedLazy", json_value) -> 
      field_options_set_unverified_lazy v (Pbrt_yojson.bool json_value "field_options" "unverified_lazy")
    | ("deprecated", json_value) -> 
      field_options_set_deprecated v (Pbrt_yojson.bool json_value "field_options" "deprecated")
    | ("weak", json_value) -> 
      field_options_set_weak v (Pbrt_yojson.bool json_value "field_options" "weak")
    | ("uninterpretedOption", `List l) -> begin
      field_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    ctype = v.ctype;
    packed = v.packed;
    jstype = v.jstype;
    lazy_ = v.lazy_;
    unverified_lazy = v.unverified_lazy;
    deprecated = v.deprecated;
    weak = v.weak;
    uninterpreted_option = v.uninterpreted_option;
  } : field_options)

let rec decode_json_field_descriptor_proto d =
  let v = default_field_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      field_descriptor_proto_set_name v (Pbrt_yojson.string json_value "field_descriptor_proto" "name")
    | ("number", json_value) -> 
      field_descriptor_proto_set_number v (Pbrt_yojson.int32 json_value "field_descriptor_proto" "number")
    | ("label", json_value) -> 
      field_descriptor_proto_set_label v ((decode_json_field_descriptor_proto_label json_value))
    | ("type", json_value) -> 
      field_descriptor_proto_set_type_ v ((decode_json_field_descriptor_proto_type json_value))
    | ("typeName", json_value) -> 
      field_descriptor_proto_set_type_name v (Pbrt_yojson.string json_value "field_descriptor_proto" "type_name")
    | ("extendee", json_value) -> 
      field_descriptor_proto_set_extendee v (Pbrt_yojson.string json_value "field_descriptor_proto" "extendee")
    | ("defaultValue", json_value) -> 
      field_descriptor_proto_set_default_value v (Pbrt_yojson.string json_value "field_descriptor_proto" "default_value")
    | ("oneofIndex", json_value) -> 
      field_descriptor_proto_set_oneof_index v (Pbrt_yojson.int32 json_value "field_descriptor_proto" "oneof_index")
    | ("jsonName", json_value) -> 
      field_descriptor_proto_set_json_name v (Pbrt_yojson.string json_value "field_descriptor_proto" "json_name")
    | ("options", json_value) -> 
      field_descriptor_proto_set_options v (decode_json_field_options json_value)
    | ("proto3Optional", json_value) -> 
      field_descriptor_proto_set_proto3_optional v (Pbrt_yojson.bool json_value "field_descriptor_proto" "proto3_optional")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    number = v.number;
    label = v.label;
    type_ = v.type_;
    type_name = v.type_name;
    extendee = v.extendee;
    default_value = v.default_value;
    oneof_index = v.oneof_index;
    json_name = v.json_name;
    options = v.options;
    proto3_optional = v.proto3_optional;
  } : field_descriptor_proto)

let rec decode_json_enum_value_options d =
  let v = default_enum_value_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("deprecated", json_value) -> 
      enum_value_options_set_deprecated v (Pbrt_yojson.bool json_value "enum_value_options" "deprecated")
    | ("uninterpretedOption", `List l) -> begin
      enum_value_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    deprecated = v.deprecated;
    uninterpreted_option = v.uninterpreted_option;
  } : enum_value_options)

let rec decode_json_enum_value_descriptor_proto d =
  let v = default_enum_value_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      enum_value_descriptor_proto_set_name v (Pbrt_yojson.string json_value "enum_value_descriptor_proto" "name")
    | ("number", json_value) -> 
      enum_value_descriptor_proto_set_number v (Pbrt_yojson.int32 json_value "enum_value_descriptor_proto" "number")
    | ("options", json_value) -> 
      enum_value_descriptor_proto_set_options v (decode_json_enum_value_options json_value)
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    number = v.number;
    options = v.options;
  } : enum_value_descriptor_proto)

let rec decode_json_enum_options d =
  let v = default_enum_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("allowAlias", json_value) -> 
      enum_options_set_allow_alias v (Pbrt_yojson.bool json_value "enum_options" "allow_alias")
    | ("deprecated", json_value) -> 
      enum_options_set_deprecated v (Pbrt_yojson.bool json_value "enum_options" "deprecated")
    | ("uninterpretedOption", `List l) -> begin
      enum_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    allow_alias = v.allow_alias;
    deprecated = v.deprecated;
    uninterpreted_option = v.uninterpreted_option;
  } : enum_options)

let rec decode_json_enum_descriptor_proto_enum_reserved_range d =
  let v = default_enum_descriptor_proto_enum_reserved_range () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("start", json_value) -> 
      enum_descriptor_proto_enum_reserved_range_set_start v (Pbrt_yojson.int32 json_value "enum_descriptor_proto_enum_reserved_range" "start")
    | ("end", json_value) -> 
      enum_descriptor_proto_enum_reserved_range_set_end_ v (Pbrt_yojson.int32 json_value "enum_descriptor_proto_enum_reserved_range" "end_")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    start = v.start;
    end_ = v.end_;
  } : enum_descriptor_proto_enum_reserved_range)

let rec decode_json_enum_descriptor_proto d =
  let v = default_enum_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      enum_descriptor_proto_set_name v (Pbrt_yojson.string json_value "enum_descriptor_proto" "name")
    | ("value", `List l) -> begin
      enum_descriptor_proto_set_value v @@ List.map (function
        | json_value -> (decode_json_enum_value_descriptor_proto json_value)
      ) l;
    end
    | ("options", json_value) -> 
      enum_descriptor_proto_set_options v (decode_json_enum_options json_value)
    | ("reservedRange", `List l) -> begin
      enum_descriptor_proto_set_reserved_range v @@ List.map (function
        | json_value -> (decode_json_enum_descriptor_proto_enum_reserved_range json_value)
      ) l;
    end
    | ("reservedName", `List l) -> begin
      enum_descriptor_proto_set_reserved_name v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "enum_descriptor_proto" "reserved_name"
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    value = v.value;
    options = v.options;
    reserved_range = v.reserved_range;
    reserved_name = v.reserved_name;
  } : enum_descriptor_proto)

let rec decode_json_extension_range_options d =
  let v = default_extension_range_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("uninterpretedOption", `List l) -> begin
      extension_range_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    uninterpreted_option = v.uninterpreted_option;
  } : extension_range_options)

let rec decode_json_descriptor_proto_extension_range d =
  let v = default_descriptor_proto_extension_range () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("start", json_value) -> 
      descriptor_proto_extension_range_set_start v (Pbrt_yojson.int32 json_value "descriptor_proto_extension_range" "start")
    | ("end", json_value) -> 
      descriptor_proto_extension_range_set_end_ v (Pbrt_yojson.int32 json_value "descriptor_proto_extension_range" "end_")
    | ("options", json_value) -> 
      descriptor_proto_extension_range_set_options v (decode_json_extension_range_options json_value)
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    start = v.start;
    end_ = v.end_;
    options = v.options;
  } : descriptor_proto_extension_range)

let rec decode_json_oneof_options d =
  let v = default_oneof_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("uninterpretedOption", `List l) -> begin
      oneof_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    uninterpreted_option = v.uninterpreted_option;
  } : oneof_options)

let rec decode_json_oneof_descriptor_proto d =
  let v = default_oneof_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      oneof_descriptor_proto_set_name v (Pbrt_yojson.string json_value "oneof_descriptor_proto" "name")
    | ("options", json_value) -> 
      oneof_descriptor_proto_set_options v (decode_json_oneof_options json_value)
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    options = v.options;
  } : oneof_descriptor_proto)

let rec decode_json_message_options d =
  let v = default_message_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("messageSetWireFormat", json_value) -> 
      message_options_set_message_set_wire_format v (Pbrt_yojson.bool json_value "message_options" "message_set_wire_format")
    | ("noStandardDescriptorAccessor", json_value) -> 
      message_options_set_no_standard_descriptor_accessor v (Pbrt_yojson.bool json_value "message_options" "no_standard_descriptor_accessor")
    | ("deprecated", json_value) -> 
      message_options_set_deprecated v (Pbrt_yojson.bool json_value "message_options" "deprecated")
    | ("mapEntry", json_value) -> 
      message_options_set_map_entry v (Pbrt_yojson.bool json_value "message_options" "map_entry")
    | ("uninterpretedOption", `List l) -> begin
      message_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    message_set_wire_format = v.message_set_wire_format;
    no_standard_descriptor_accessor = v.no_standard_descriptor_accessor;
    deprecated = v.deprecated;
    map_entry = v.map_entry;
    uninterpreted_option = v.uninterpreted_option;
  } : message_options)

let rec decode_json_descriptor_proto_reserved_range d =
  let v = default_descriptor_proto_reserved_range () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("start", json_value) -> 
      descriptor_proto_reserved_range_set_start v (Pbrt_yojson.int32 json_value "descriptor_proto_reserved_range" "start")
    | ("end", json_value) -> 
      descriptor_proto_reserved_range_set_end_ v (Pbrt_yojson.int32 json_value "descriptor_proto_reserved_range" "end_")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    start = v.start;
    end_ = v.end_;
  } : descriptor_proto_reserved_range)

let rec decode_json_descriptor_proto d =
  let v = default_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      descriptor_proto_set_name v (Pbrt_yojson.string json_value "descriptor_proto" "name")
    | ("field", `List l) -> begin
      descriptor_proto_set_field v @@ List.map (function
        | json_value -> (decode_json_field_descriptor_proto json_value)
      ) l;
    end
    | ("extension", `List l) -> begin
      descriptor_proto_set_extension v @@ List.map (function
        | json_value -> (decode_json_field_descriptor_proto json_value)
      ) l;
    end
    | ("nestedType", `List l) -> begin
      descriptor_proto_set_nested_type v @@ List.map (function
        | json_value -> (decode_json_descriptor_proto json_value)
      ) l;
    end
    | ("enumType", `List l) -> begin
      descriptor_proto_set_enum_type v @@ List.map (function
        | json_value -> (decode_json_enum_descriptor_proto json_value)
      ) l;
    end
    | ("extensionRange", `List l) -> begin
      descriptor_proto_set_extension_range v @@ List.map (function
        | json_value -> (decode_json_descriptor_proto_extension_range json_value)
      ) l;
    end
    | ("oneofDecl", `List l) -> begin
      descriptor_proto_set_oneof_decl v @@ List.map (function
        | json_value -> (decode_json_oneof_descriptor_proto json_value)
      ) l;
    end
    | ("options", json_value) -> 
      descriptor_proto_set_options v (decode_json_message_options json_value)
    | ("reservedRange", `List l) -> begin
      descriptor_proto_set_reserved_range v @@ List.map (function
        | json_value -> (decode_json_descriptor_proto_reserved_range json_value)
      ) l;
    end
    | ("reservedName", `List l) -> begin
      descriptor_proto_set_reserved_name v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "descriptor_proto" "reserved_name"
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    field = v.field;
    extension = v.extension;
    nested_type = v.nested_type;
    enum_type = v.enum_type;
    extension_range = v.extension_range;
    oneof_decl = v.oneof_decl;
    options = v.options;
    reserved_range = v.reserved_range;
    reserved_name = v.reserved_name;
  } : descriptor_proto)

let rec decode_json_method_options_idempotency_level json =
  match json with
  | `String "IDEMPOTENCY_UNKNOWN" -> (Idempotency_unknown : method_options_idempotency_level)
  | `String "NO_SIDE_EFFECTS" -> (No_side_effects : method_options_idempotency_level)
  | `String "IDEMPOTENT" -> (Idempotent : method_options_idempotency_level)
  | _ -> Pbrt_yojson.E.malformed_variant "method_options_idempotency_level"

let rec decode_json_method_options d =
  let v = default_method_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("deprecated", json_value) -> 
      method_options_set_deprecated v (Pbrt_yojson.bool json_value "method_options" "deprecated")
    | ("idempotencyLevel", json_value) -> 
      method_options_set_idempotency_level v ((decode_json_method_options_idempotency_level json_value))
    | ("uninterpretedOption", `List l) -> begin
      method_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    deprecated = v.deprecated;
    idempotency_level = v.idempotency_level;
    uninterpreted_option = v.uninterpreted_option;
  } : method_options)

let rec decode_json_method_descriptor_proto d =
  let v = default_method_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      method_descriptor_proto_set_name v (Pbrt_yojson.string json_value "method_descriptor_proto" "name")
    | ("inputType", json_value) -> 
      method_descriptor_proto_set_input_type v (Pbrt_yojson.string json_value "method_descriptor_proto" "input_type")
    | ("outputType", json_value) -> 
      method_descriptor_proto_set_output_type v (Pbrt_yojson.string json_value "method_descriptor_proto" "output_type")
    | ("options", json_value) -> 
      method_descriptor_proto_set_options v (decode_json_method_options json_value)
    | ("clientStreaming", json_value) -> 
      method_descriptor_proto_set_client_streaming v (Pbrt_yojson.bool json_value "method_descriptor_proto" "client_streaming")
    | ("serverStreaming", json_value) -> 
      method_descriptor_proto_set_server_streaming v (Pbrt_yojson.bool json_value "method_descriptor_proto" "server_streaming")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    input_type = v.input_type;
    output_type = v.output_type;
    options = v.options;
    client_streaming = v.client_streaming;
    server_streaming = v.server_streaming;
  } : method_descriptor_proto)

let rec decode_json_service_options d =
  let v = default_service_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("deprecated", json_value) -> 
      service_options_set_deprecated v (Pbrt_yojson.bool json_value "service_options" "deprecated")
    | ("uninterpretedOption", `List l) -> begin
      service_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    deprecated = v.deprecated;
    uninterpreted_option = v.uninterpreted_option;
  } : service_options)

let rec decode_json_service_descriptor_proto d =
  let v = default_service_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      service_descriptor_proto_set_name v (Pbrt_yojson.string json_value "service_descriptor_proto" "name")
    | ("method", `List l) -> begin
      service_descriptor_proto_set_method_ v @@ List.map (function
        | json_value -> (decode_json_method_descriptor_proto json_value)
      ) l;
    end
    | ("options", json_value) -> 
      service_descriptor_proto_set_options v (decode_json_service_options json_value)
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    method_ = v.method_;
    options = v.options;
  } : service_descriptor_proto)

let rec decode_json_file_options_optimize_mode json =
  match json with
  | `String "SPEED" -> (Speed : file_options_optimize_mode)
  | `String "CODE_SIZE" -> (Code_size : file_options_optimize_mode)
  | `String "LITE_RUNTIME" -> (Lite_runtime : file_options_optimize_mode)
  | _ -> Pbrt_yojson.E.malformed_variant "file_options_optimize_mode"

let rec decode_json_file_options d =
  let v = default_file_options () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("javaPackage", json_value) -> 
      file_options_set_java_package v (Pbrt_yojson.string json_value "file_options" "java_package")
    | ("javaOuterClassname", json_value) -> 
      file_options_set_java_outer_classname v (Pbrt_yojson.string json_value "file_options" "java_outer_classname")
    | ("javaMultipleFiles", json_value) -> 
      file_options_set_java_multiple_files v (Pbrt_yojson.bool json_value "file_options" "java_multiple_files")
    | ("javaGenerateEqualsAndHash", json_value) -> 
      file_options_set_java_generate_equals_and_hash v (Pbrt_yojson.bool json_value "file_options" "java_generate_equals_and_hash")
    | ("javaStringCheckUtf8", json_value) -> 
      file_options_set_java_string_check_utf8 v (Pbrt_yojson.bool json_value "file_options" "java_string_check_utf8")
    | ("optimizeFor", json_value) -> 
      file_options_set_optimize_for v ((decode_json_file_options_optimize_mode json_value))
    | ("goPackage", json_value) -> 
      file_options_set_go_package v (Pbrt_yojson.string json_value "file_options" "go_package")
    | ("ccGenericServices", json_value) -> 
      file_options_set_cc_generic_services v (Pbrt_yojson.bool json_value "file_options" "cc_generic_services")
    | ("javaGenericServices", json_value) -> 
      file_options_set_java_generic_services v (Pbrt_yojson.bool json_value "file_options" "java_generic_services")
    | ("pyGenericServices", json_value) -> 
      file_options_set_py_generic_services v (Pbrt_yojson.bool json_value "file_options" "py_generic_services")
    | ("phpGenericServices", json_value) -> 
      file_options_set_php_generic_services v (Pbrt_yojson.bool json_value "file_options" "php_generic_services")
    | ("deprecated", json_value) -> 
      file_options_set_deprecated v (Pbrt_yojson.bool json_value "file_options" "deprecated")
    | ("ccEnableArenas", json_value) -> 
      file_options_set_cc_enable_arenas v (Pbrt_yojson.bool json_value "file_options" "cc_enable_arenas")
    | ("objcClassPrefix", json_value) -> 
      file_options_set_objc_class_prefix v (Pbrt_yojson.string json_value "file_options" "objc_class_prefix")
    | ("csharpNamespace", json_value) -> 
      file_options_set_csharp_namespace v (Pbrt_yojson.string json_value "file_options" "csharp_namespace")
    | ("swiftPrefix", json_value) -> 
      file_options_set_swift_prefix v (Pbrt_yojson.string json_value "file_options" "swift_prefix")
    | ("phpClassPrefix", json_value) -> 
      file_options_set_php_class_prefix v (Pbrt_yojson.string json_value "file_options" "php_class_prefix")
    | ("phpNamespace", json_value) -> 
      file_options_set_php_namespace v (Pbrt_yojson.string json_value "file_options" "php_namespace")
    | ("phpMetadataNamespace", json_value) -> 
      file_options_set_php_metadata_namespace v (Pbrt_yojson.string json_value "file_options" "php_metadata_namespace")
    | ("rubyPackage", json_value) -> 
      file_options_set_ruby_package v (Pbrt_yojson.string json_value "file_options" "ruby_package")
    | ("uninterpretedOption", `List l) -> begin
      file_options_set_uninterpreted_option v @@ List.map (function
        | json_value -> (decode_json_uninterpreted_option json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    java_package = v.java_package;
    java_outer_classname = v.java_outer_classname;
    java_multiple_files = v.java_multiple_files;
    java_generate_equals_and_hash = v.java_generate_equals_and_hash;
    java_string_check_utf8 = v.java_string_check_utf8;
    optimize_for = v.optimize_for;
    go_package = v.go_package;
    cc_generic_services = v.cc_generic_services;
    java_generic_services = v.java_generic_services;
    py_generic_services = v.py_generic_services;
    php_generic_services = v.php_generic_services;
    deprecated = v.deprecated;
    cc_enable_arenas = v.cc_enable_arenas;
    objc_class_prefix = v.objc_class_prefix;
    csharp_namespace = v.csharp_namespace;
    swift_prefix = v.swift_prefix;
    php_class_prefix = v.php_class_prefix;
    php_namespace = v.php_namespace;
    php_metadata_namespace = v.php_metadata_namespace;
    ruby_package = v.ruby_package;
    uninterpreted_option = v.uninterpreted_option;
  } : file_options)

let rec decode_json_source_code_info_location d =
  let v = default_source_code_info_location () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("path", `List l) -> begin
      source_code_info_location_set_path v @@ List.map (function
        | json_value -> Pbrt_yojson.int32 json_value "source_code_info_location" "path"
      ) l;
    end
    | ("span", `List l) -> begin
      source_code_info_location_set_span v @@ List.map (function
        | json_value -> Pbrt_yojson.int32 json_value "source_code_info_location" "span"
      ) l;
    end
    | ("leadingComments", json_value) -> 
      source_code_info_location_set_leading_comments v (Pbrt_yojson.string json_value "source_code_info_location" "leading_comments")
    | ("trailingComments", json_value) -> 
      source_code_info_location_set_trailing_comments v (Pbrt_yojson.string json_value "source_code_info_location" "trailing_comments")
    | ("leadingDetachedComments", `List l) -> begin
      source_code_info_location_set_leading_detached_comments v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "source_code_info_location" "leading_detached_comments"
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    path = v.path;
    span = v.span;
    leading_comments = v.leading_comments;
    trailing_comments = v.trailing_comments;
    leading_detached_comments = v.leading_detached_comments;
  } : source_code_info_location)

let rec decode_json_source_code_info d =
  let v = default_source_code_info () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("location", `List l) -> begin
      source_code_info_set_location v @@ List.map (function
        | json_value -> (decode_json_source_code_info_location json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    location = v.location;
  } : source_code_info)

let rec decode_json_file_descriptor_proto d =
  let v = default_file_descriptor_proto () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("name", json_value) -> 
      file_descriptor_proto_set_name v (Pbrt_yojson.string json_value "file_descriptor_proto" "name")
    | ("package", json_value) -> 
      file_descriptor_proto_set_package v (Pbrt_yojson.string json_value "file_descriptor_proto" "package")
    | ("dependency", `List l) -> begin
      file_descriptor_proto_set_dependency v @@ List.map (function
        | json_value -> Pbrt_yojson.string json_value "file_descriptor_proto" "dependency"
      ) l;
    end
    | ("publicDependency", `List l) -> begin
      file_descriptor_proto_set_public_dependency v @@ List.map (function
        | json_value -> Pbrt_yojson.int32 json_value "file_descriptor_proto" "public_dependency"
      ) l;
    end
    | ("weakDependency", `List l) -> begin
      file_descriptor_proto_set_weak_dependency v @@ List.map (function
        | json_value -> Pbrt_yojson.int32 json_value "file_descriptor_proto" "weak_dependency"
      ) l;
    end
    | ("messageType", `List l) -> begin
      file_descriptor_proto_set_message_type v @@ List.map (function
        | json_value -> (decode_json_descriptor_proto json_value)
      ) l;
    end
    | ("enumType", `List l) -> begin
      file_descriptor_proto_set_enum_type v @@ List.map (function
        | json_value -> (decode_json_enum_descriptor_proto json_value)
      ) l;
    end
    | ("service", `List l) -> begin
      file_descriptor_proto_set_service v @@ List.map (function
        | json_value -> (decode_json_service_descriptor_proto json_value)
      ) l;
    end
    | ("extension", `List l) -> begin
      file_descriptor_proto_set_extension v @@ List.map (function
        | json_value -> (decode_json_field_descriptor_proto json_value)
      ) l;
    end
    | ("options", json_value) -> 
      file_descriptor_proto_set_options v (decode_json_file_options json_value)
    | ("sourceCodeInfo", json_value) -> 
      file_descriptor_proto_set_source_code_info v (decode_json_source_code_info json_value)
    | ("syntax", json_value) -> 
      file_descriptor_proto_set_syntax v (Pbrt_yojson.string json_value "file_descriptor_proto" "syntax")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    name = v.name;
    package = v.package;
    dependency = v.dependency;
    public_dependency = v.public_dependency;
    weak_dependency = v.weak_dependency;
    message_type = v.message_type;
    enum_type = v.enum_type;
    service = v.service;
    extension = v.extension;
    options = v.options;
    source_code_info = v.source_code_info;
    syntax = v.syntax;
  } : file_descriptor_proto)

let rec decode_json_file_descriptor_set d =
  let v = default_file_descriptor_set () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("file", `List l) -> begin
      file_descriptor_set_set_file v @@ List.map (function
        | json_value -> (decode_json_file_descriptor_proto json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    file = v.file;
  } : file_descriptor_set)

let rec decode_json_generated_code_info_annotation d =
  let v = default_generated_code_info_annotation () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("path", `List l) -> begin
      generated_code_info_annotation_set_path v @@ List.map (function
        | json_value -> Pbrt_yojson.int32 json_value "generated_code_info_annotation" "path"
      ) l;
    end
    | ("sourceFile", json_value) -> 
      generated_code_info_annotation_set_source_file v (Pbrt_yojson.string json_value "generated_code_info_annotation" "source_file")
    | ("begin", json_value) -> 
      generated_code_info_annotation_set_begin_ v (Pbrt_yojson.int32 json_value "generated_code_info_annotation" "begin_")
    | ("end", json_value) -> 
      generated_code_info_annotation_set_end_ v (Pbrt_yojson.int32 json_value "generated_code_info_annotation" "end_")
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    _presence = v._presence;
    path = v.path;
    source_file = v.source_file;
    begin_ = v.begin_;
    end_ = v.end_;
  } : generated_code_info_annotation)

let rec decode_json_generated_code_info d =
  let v = default_generated_code_info () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("annotation", `List l) -> begin
      generated_code_info_set_annotation v @@ List.map (function
        | json_value -> (decode_json_generated_code_info_annotation json_value)
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    annotation = v.annotation;
  } : generated_code_info)
