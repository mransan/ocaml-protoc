let parse s  = 
  Pb_parsing_parser.extend_ Pb_parsing_lexer.lexer (Lexing.from_string s)

let () = 
  let s ="extend M {} " in 
  let ev = parse s in 
  assert("M" = ev.Pbpt.extend_name); 
  assert([]  = ev.Pbpt.extend_body); 
  ()

let () = 
  let s ="extend M {optional int32 i = 1; ; }; " in 
  let ev = parse s in 
  assert("M" = ev.Pbpt.extend_name); 
  begin 
    match ev.Pbpt.extend_body with
    | {Pbpt.field_name; field_number; field_label; field_type; field_options} :: [] -> (
      assert(field_name = "i");
      assert(field_number = 1);
      assert(field_type = "int32");
      assert(field_options = []);
      assert(`Optional = field_label);
      ()
    )
    | _ -> (assert false : unit) 
  end;
  ()

let () = 
  let s ="extend M {
    optional int32 i = 1;
    required double j = 2; 
  } " in 
  let ev = parse s in 
  assert("M" = ev.Pbpt.extend_name); 
  begin 
    match ev.Pbpt.extend_body with
    | {Pbpt.field_name; field_number; field_label; field_type; field_options} :: f2 :: [] -> (
      assert(field_name = "i");
      assert(field_number = 1);
      assert(field_type = "int32");
      assert(field_options = []);
      assert(`Optional = field_label);
      let {Pbpt.field_name; field_number; field_label; field_type; field_options}  = f2 in 
      assert(field_name = "j");
      assert(field_number = 2);
      assert(field_type = "double");
      assert(field_options = []);
      assert(`Required = field_label);
      ()
    )
    | _ -> (assert false : unit) 
  end;
  ()


let () = 
  print_endline "Parse Extend ... Ok" 



