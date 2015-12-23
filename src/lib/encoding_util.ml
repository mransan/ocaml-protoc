type payload_kind = 
  | Varint of bool (** zigzag *)  
  | Bits32
  | Bits64
  | Bytes 

type other_module_location = {
  file_name: string; 
  type_name: string; 
}


type field_encoding = {
  field_number: int; 
  payload_kind: payload_kind; 
  nested: bool;
  default: Pbpt.constant option;
}

let string_of_payload_kind = function 
  | Varint _ -> "varint"
  | Bits32   -> "bits32"
  | Bits64   -> "bits64"
  | Bytes    -> "bytes"

let encoding_of_field_type all_types (field:(Pbtt.resolved, 'a) Pbtt.field) = 

  let pk, nested = match Pbtt_util.field_type field with 
    | Pbtt.Field_type_double  -> (Bits64,false)
    | Pbtt.Field_type_float  -> (Bits32 ,false)
    | Pbtt.Field_type_int32  -> (Varint false,false)
    | Pbtt.Field_type_int64  -> (Varint false,false)
    | Pbtt.Field_type_uint32  -> (Varint false,false)
    | Pbtt.Field_type_uint64 -> (Varint false,false)
    | Pbtt.Field_type_sint32  -> (Varint true,false)
    | Pbtt.Field_type_sint64  -> (Varint true ,false)
    | Pbtt.Field_type_fixed32  -> (Bits32,false)
    | Pbtt.Field_type_fixed64  -> (Bits64,false)
    | Pbtt.Field_type_sfixed32  -> (Bits32,false)
    | Pbtt.Field_type_sfixed64 -> (Bits64,false)
    | Pbtt.Field_type_bool  -> (Varint false ,false)
    | Pbtt.Field_type_string  -> (Bytes,false)
    | Pbtt.Field_type_bytes  -> (Bytes,false)
    | Pbtt.Field_type_type id -> 
      match Pbtt_util.type_of_id all_types id with 
      | {Pbtt.spec = Pbtt.Enum    {Pbtt.enum_name; _ } ;Pbtt.file_name; _ }   -> (Varint false, false)
      | {Pbtt.spec = Pbtt.Message {Pbtt.message_name; _ } ;Pbtt.file_name; _} -> (Bytes, true)
  in {
    payload_kind = pk;
    nested; 
    field_number = Pbtt_util.field_number field;
    default = Pbtt_util.field_default field;
  }
