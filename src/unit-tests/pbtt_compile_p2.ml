
let parse f s  = 
  f Lexer.lexer (Lexing.from_string s)

let () = 
  let s = "
  message M1 {
    message M2 {
      message M21 {
      }
    }
    message M3 { 
     
      required M1.M2      x1 = 1; 
      required M1.M2.M21  x2 = 2; 
      required M1.M3      x3 = 3; 
      required M1         x4 = 4; 
      required M3         x5 = 5; 
      required M2         x6 = 6; 
      required M2.M21     x7 = 7; 
      oneof x {
        M1.M2      xo1  = 11; 
        M1.M2.M21  xo2  = 12; 
        M1.M3      xo3  = 13; 
        M1         xo4  = 14; 
        M3         xo5  = 15; 
        M2         xo6  = 16; 
        M2.M21     xo7  = 17; 
        .M1        xo8  = 18; 
        .M1.M2     xo9  = 19; 
        .M1.M2.M21 xo10 = 110; 
      }
    }
    required M2 vm2 = 1;
  }
  " in 
  let ast = parse Parser.message_ s in 
  let all_types = Pbtt_util.compile_message_p1 Pbtt_util.empty_scope ast in 
  ignore @@ List.map (function 
    | Pbtt.Message m  -> Pbtt_util.compile_message_p2 all_types m) all_types; 
  ()

let assert_unresolved f = 
  match ignore @@ f () with 
  | exception (Exception.Compilation_error (Exception.Unresolved_type _) )-> () 
  | _ -> assert(false)

let test_unresolved_msg s = 
  let ast = parse Parser.message_ s in 
  let all_types = Pbtt_util.compile_message_p1 Pbtt_util.empty_scope ast in 
  assert_unresolved (fun () -> 
    ignore @@ List.map (function | Pbtt.Message m -> Pbtt_util.compile_message_p2 all_types m) all_types
  )

let () = 
  let s = "
  message M1 {
    required Dont.Exist x1 = 1; 
  }
  " in 
  test_unresolved_msg s;
  ()

let () = 
  let s = "
  message M1 {
    required M1.M2 x1 = 1; 
  }
  " in 
  test_unresolved_msg s; 
  ()

let () = 
  let s = "
  message M1 {
    message M2 {} 
    required M1.M3 x1 = 1; 
  }
  " in 
  test_unresolved_msg s; 
  ()
 
let () = 
  let s = "
  message M1 {
    message M2 {} 
    required .M2 x1 = 1; 
  }
  " in 
  test_unresolved_msg s; 
  ()
 

let assert_duplicate f = 
  match ignore @@ f () with 
  | exception (Exception.Compilation_error (Exception.Duplicated_field_number _) )-> () 
  | _ -> assert(false)


let test_duplicate s = 
  let ast = parse Parser.message_ s in 
  assert_duplicate (fun () -> 
    let all_types = Pbtt_util.compile_message_p1 Pbtt_util.empty_scope ast in 
    ignore @@ List.map (function |Pbtt.Message m -> Pbtt_util.compile_message_p2 all_types m) all_types
  )

let () = 
  let s = "
  message M1 {
    required uint32 x = 1; 
    required uint32 y = 1; 
  }
  " in 
  test_duplicate s; 
  ()
 
let () = 
  let s = "
  message M1 {
    required uint32 x = 100;
    required uint32 y = 1; 
    required uint32 z = 100; 
  }
  " in 
  test_duplicate s; 
  ()
 
let () = 
  let s = "
  message M1 {
    required uint32 x = 1;
    oneof o {
      uint32 y = 1;
    }
  }
  " in 
  test_duplicate s; 
  ()
 
let () = 
  let s = "
  message M1 {
    oneof o {
      uint32 y = 1;
    }
    required uint32 x = 1;
  }
  " in 
  test_duplicate s; 
  ()
 
let () = 
  let s = "
  message M1 {
    oneof o {
      uint32 x = 1;
    }
    oneof o {
      uint32 y = 1;
    }
  }
  " in 
  test_duplicate s; 
  ()
 
let () = 
  let s = "
  message M1 {
    oneof o {
      uint32 y = 1;
    }
    oneof o {
      uint32 y = 2;
    }
  }
  " in 
  test_duplicate s; 
  ()
 
let () = 
  print_endline "Pbtt Compile P2 ... Ok"
