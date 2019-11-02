open Example04_types
open Example04_pp

let (@+) value next = 
  Cons {value;next}

let () = 
  let l = 1 @+ 2 @+ 3 @+ 4 @+ 5 @+ Nil in 
  Format.(fprintf std_formatter "l = %a\n" pp_int_list l)
