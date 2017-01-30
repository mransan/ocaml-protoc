type constant = 
  | Constant_string of string 
  | Constant_bool of bool 
  | Constant_int of int 
  | Constant_float of float 
  | Constant_litteral of string 

type option_name = string 

type t = (option_name * constant) 

type set = t list  

let empty = []

let add t option_name constant = 
  (option_name, constant) :: t 

let merge t1 t2 = 
  t2 @ t1 

let get t option_name = 
  match List.assoc option_name t with 
  | c -> Some c 
  | exception Not_found -> None 

