
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
  field_number : int; 
  payload_kind : payload_kind; 
  nested : bool;
}

val string_of_payload_kind : payload_kind -> string 

val encoding_of_field_type : Pbtt.resolved Pbtt.proto -> (Pbtt.resolved, 'a) Pbtt.field -> field_encoding 
