module Tt = Pb_typing_type_tree 

type type_ = Tt.resolved_field_type Tt.proto_type

let perform_typing protos = 

  let typed_protos = List.fold_left (fun pbtt_msgs proto -> 
    pbtt_msgs @ Pb_typing_validation.validate proto
  ) [] protos in   

  let typed_protos = Pb_typing_resolution.resolve_types typed_protos in 

  List.rev @@ Pb_typing_recursion.group typed_protos
