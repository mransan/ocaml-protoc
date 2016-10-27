let parse s  = 
  Pbparser.extension_range_list_ Pblexer.lexer (Lexing.from_string s)

let parse_extension s  = 
  Pbparser.extension_ Pblexer.lexer (Lexing.from_string s)

let parse_reserved s  = 
  Pbparser.reserved_ Pblexer.lexer (Lexing.from_string s)

let () = 
  let s ="1" in 
  let ev = parse s in 
  assert(Pbpt.Extension_single_number 1 = List.hd ev); 
  ()

let () = 
  let s ="1 to 2" in 
  let ev = parse s in 
  assert(Pbpt.Extension_range (1, Pbpt.To_number 2) = List.hd ev); 
  ()

let () = 
  let s ="1 to -1" in 
  let ev = parse s in 
  assert(Pbpt.Extension_range (1, Pbpt.To_number (-1)) = List.hd ev); 
  ()

let () = 
  let s ="1 to max" in 
  let ev = parse s in 
  assert(Pbpt.Extension_range (1, Pbpt.To_max) = List.hd ev); 
  ()

let () = 
  let s  = "1,2,3 to 10, 11 to max" in 
  let ev = parse s in 
  begin 
    match ev with
    | ev1::ev2::ev3::ev4::[] -> (
      assert(Pbpt.Extension_single_number 1 = ev1); 
      assert(Pbpt.Extension_single_number 2 = ev2); 
      assert(Pbpt.Extension_range (3, Pbpt.To_number 10) = ev3); 
      assert(Pbpt.Extension_range (11, Pbpt.To_max) = ev4)
    ) 
    | _ -> (assert false: unit)
  end;
  ()

let () = 
  let s  = "extensions 1,2,3 to 10, 11 to max;" in 
  let ev = parse_extension s in 
  begin 
    match ev with
    | ev1::ev2::ev3::ev4::[] -> (
      assert(Pbpt.Extension_single_number 1 = ev1); 
      assert(Pbpt.Extension_single_number 2 = ev2); 
      assert(Pbpt.Extension_range (3, Pbpt.To_number 10) = ev3); 
      assert(Pbpt.Extension_range (11, Pbpt.To_max) = ev4)
    ) 
    | _ -> (assert false: unit)
  end;
  ()

let () = 
  let s  = "reserved 1,2,3 to 10, 11 to max;" in 
  let ev = parse_reserved s in 
  begin 
    match ev with
    | ev1::ev2::ev3::ev4::[] -> (
      assert(Pbpt.Extension_single_number 1 = ev1); 
      assert(Pbpt.Extension_single_number 2 = ev2); 
      assert(Pbpt.Extension_range (3, Pbpt.To_number 10) = ev3); 
      assert(Pbpt.Extension_range (11, Pbpt.To_max) = ev4)
    ) 
    | _ -> (assert false: unit)
  end;
  ()

let test_failure f = 
  match f() with 
  | x -> (assert false:unit)
  | exception (Failure _) -> () 
  | exception (Parsing.Parse_error) -> () 

let () = 
  let s  = "1 until 2" in
  test_failure (fun () -> parse s); 
  ()

let () = 
  let s  = "1 to min" in
  test_failure (fun () -> parse s); 
  ()

let () = 
  print_endline "Parse Extension Range... Ok" 

