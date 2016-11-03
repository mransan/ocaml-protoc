
module Pt = Pb_parsing_parse_tree 
module Tt = Pb_typing_type_tree 

val validate : 
  Pt.proto ->
  Tt.unresolved_field_type Tt.proto
(** [validate file_name proto] makes a first phase compilation of the 
    parsed tree. 
 *)


(** {2 For testing only} *) 

val validate_message : 
  ?parent_options:Pt.message_option list ->
  string -> 
  Pt.file_option list ->
  Tt.type_scope -> 
  Pt.message ->
  Tt.unresolved_field_type Tt.proto

