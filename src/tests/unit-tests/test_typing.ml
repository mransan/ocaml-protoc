let is_found t path type_name = 
  match Pb_typing_resolution.Types_by_scope.find t path type_name  with
  | exception Not_found -> false 
  | _ -> true 

let () =
  let proto_file = "\
package foo.bar;\
message M1 { message M11 {} message M12 {} } \
message M2 { message M21 {} message M22 {} } \
"
  in
  
  let proto = Pb_parsing_parser.proto_ Pb_parsing_lexer.lexer (Lexing.from_string proto_file) in
  let proto = Pb_typing_validation.validate proto in 
  let t = List.fold_left  (fun t type_ -> 
    Pb_typing_resolution.Types_by_scope.add t type_
  ) (Pb_typing_resolution.Types_by_scope.empty) proto in 
  assert(is_found t ["foo"; "bar"] "M1");
  assert(is_found t ["foo"; "bar"] "M2");
  assert(is_found t ["foo"; "bar"; "M1"] "M11");
  assert(is_found t ["foo"; "bar"; "M1"] "M11");
  assert(is_found t ["foo"; "bar"; "M2"] "M21");
  assert(is_found t ["foo"; "bar"; "M2"] "M22");
  assert(not @@ is_found t ["foo"; "bar"; "M2"] "M23");
  ()
