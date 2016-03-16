open Example03_pb

let () = 
  let v = Example03_pb.(Some "This is an example") in  
  Format.fprintf Format.std_formatter "%a\n" Example03_pb.pp_string_some v
