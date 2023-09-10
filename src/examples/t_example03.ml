let () =
  let v = Example03.Some "This is an example" in
  Format.fprintf Format.std_formatter "%a\n" Example03.pp_string_some v
