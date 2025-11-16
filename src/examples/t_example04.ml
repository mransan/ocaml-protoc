open Example04

let ( @+ ) value next = Cons (make_int_list_cons ~value ~next ())

let () =
  let l = 1 @+ 2 @+ 3 @+ 4 @+ 5 @+ Nil in
  Format.(fprintf std_formatter "l = %a\n" pp_int_list l)
