
let rev_split_by_char c s = 
  let rec loop i l = 
    try 
      let i' = String.index_from s i c  in 
      let s' = String.sub s i (i' - i)  in 
      loop (i'+1) (if s' = "" then l else s'::l)  
    with Not_found -> (String.sub s i (String.length s - i) ):: l 
  in 
  loop 0 []
  
(** [concat l] concatenate a string list *)
let concat = String.concat ""

let rec pop_last = function 
  | [] -> failwith "Invalid argument [] for pop_last"
  | hd::[] -> []
  | hd::tl -> hd :: (pop_last tl)

let rec apply_until f = function 
  | []  -> None 
  | hd::tl -> (match f hd with 
    | None -> apply_until f tl 
    | x    -> x
  )  

let is_list_empty = function | [] -> true | _ -> false 
