module Tt = Pb_typing_type_tree 

module Types_by_scope : sig 

  type t 

  val empty : t 

  val add : 
    t -> 
    Tt.unresolved_field_type Tt.proto_type -> 
    t 
  
  val find : 
    t -> 
    Tt.type_path -> 
    string -> 
    Tt.unresolved_field_type Tt.proto_type  

  val print : t -> unit 

end 

val resolve_types : 
  Tt.unresolved_field_type Tt.proto_type list ->
  Tt.resolved_field_type   Tt.proto_type list  
