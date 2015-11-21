
type payload_kind = 
  | Varint of bool (** zigzag *)  
  | Bits32
  | Bits64
  | Bytes 

type other_module_location = {
  file_name: string; 
  type_name: string; 
}

type location =
  | Standard_type
  | Within_same_module 
  | Other_file of other_module_location 

type field_encoding = {
  field_number : int; 
  payload_kind : payload_kind; 
  nested : bool;
  location : location;
}

val string_of_payload_kind : payload_kind -> string 

val encoding_of_field_type : Pbtt.resolved Pbtt.proto -> string -> (Pbtt.resolved, 'a) Pbtt.field -> field_encoding 
