
type file_loader = string -> string * string

val parse_file : 
  file_loader -> 
  string -> 
  Pb_parsing_parse_tree.proto list 
