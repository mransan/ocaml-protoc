let () = 
  let v = Example03_types.Some "This is an example" in  
  Format.fprintf Format.std_formatter "%a\n" Example03_pp.pp_string_some v
