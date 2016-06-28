module E = Exception 

let parse f s  = 
  f Pblexer.lexer (Lexing.from_string s)
let () = 
  let s =" 
  ENUM1 = 1;
  " in 
  begin match parse Pbparser.enum_value_ s with
  | Pbpt.Enum_value ev -> 
    assert("ENUM1" = ev.Pbpt.enum_value_name);
    assert(1       = ev.Pbpt.enum_value_int)
  | _ -> assert(false)
  end; 
  ()

let () = 
  let s =" 
  BLAH_12_BLAH = -99999; ;
  " in 
  begin match parse Pbparser.enum_value_ s with
  | Pbpt.Enum_value ev -> 
    assert("BLAH_12_BLAH" = ev.Pbpt.enum_value_name);
    assert((-99999) = ev.Pbpt.enum_value_int);
  | _ -> assert(false)
  end; 
  ()

let () = 
  let s =" 
  enum Test {
  EV1 = 1;
  EV2 = 2;
  }
  " in 
  let ev1 = Pbpt_util.enum_value ~int_value:1 "EV1" in 
  let ev2 = Pbpt_util.enum_value ~int_value:2 "EV2" in 
  let e   = parse Pbparser.enum_ s in 
  assert("Test" = e.Pbpt.enum_name); 
  assert(2 = List.length e.Pbpt.enum_body);
  assert(ev1 = List.nth e.Pbpt.enum_body 0); 
  assert(ev2 = List.nth e.Pbpt.enum_body 1); 
  ()

let () = 
  let s =" 
  enum Test {
  EV1 = 1
  EV2 = 2;
  }
  " in 
  match parse Pbparser.enum_ s with
  | x -> assert false 
  | exception E.Compilation_error _ -> ()
  | exception exn -> 
    print_endline @@ Printexc.to_string exn; 
    assert false

let () = 
  print_endline "Parse Enum ... Ok" 
