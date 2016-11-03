
module Tt = Pb_typing_type_tree 

type type_ = Tt.resolved_field_type Tt.proto_type

val perform_typing :
  Pb_parsing_parse_tree.proto list -> 
  type_ list list 
