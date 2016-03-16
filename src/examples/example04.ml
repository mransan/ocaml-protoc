
open Example04_pb

let (@+) value next = 
  Cons {value;next}

let () = 
  let l = 1 @+ 2 @+ 3 @+ 4 @+ 5 @+ Nil in 
  Format.(fprintf std_formatter "l = %a\n" pp_int_list l)
