module F = Format 

let pp_int = F.pp_print_int 

let pp_float = F.pp_print_float 

let pp_bool = F.pp_print_bool 

let pp_int32 fmt i = F.pp_print_string fmt (Int32.to_string i)  

let pp_int64 fmt i = F.pp_print_string fmt (Int32.to_string i)  

let pp_string fmt s = 
  F.fprintf fmt "\"%a\"" F.pp_print_string s

let pp_option pp_f fmt = function
  | None   -> F.fprintf fmt "@[None@]"
  | Some x -> F.fprintf fmt "@[Some(%a)@]" pp_f x 

let pp_list pp_element fmt l = 
  let rec pp_i fmt = function
    | [h]  -> Format.fprintf fmt "%a" pp_element h
    | h::t ->
      Format.fprintf fmt "%a;@,%a" pp_element h pp_i t
    | []   -> ()
  in
  F.fprintf fmt "@[<v 1>[%a@,@]]" pp_i l 

let pp_equal field_name pp_val fmt val_ = 
  F.fprintf fmt "@,@[<h>%s = %a;@]" field_name pp_val val_ 

let pp_brk pp_record (fmt:F.formatter) r : unit = 
  F.fprintf fmt "@[<v>{%a@,@]}" pp_record r  

type r = {
  x : int; 
  y : string; 
  z : string; 
}

let pp_r fmt r = 
  let pp_i fmt () = 
    F.pp_open_vbox fmt 1; 
    pp_equal "x" pp_int fmt r.x;
    pp_equal "y" pp_string fmt r.y;
    pp_equal "z" pp_string fmt r.z;
    F.pp_close_box fmt ()
  in
  pp_brk pp_i fmt () 

type r' = {
  r : r; 
  i : int option; 
  l : float list;
}

let pp_r' fmt r' = 
  let pp_i fmt () =
    F.pp_open_vbox fmt 1; 
    pp_equal "r" pp_r fmt r'.r ;
    pp_equal "i" (pp_option pp_int) fmt r'.i;
    pp_equal "l" (pp_list pp_float) fmt r'.l;
    F.pp_close_box fmt () 
  in
  pp_brk pp_i fmt ()

type or_ = 
  | R  of r 
  | Rp of r' list 

let pp_or_ fmt = function
  | R  x -> F.fprintf fmt "@[R(%a)@]" pp_r x 
  | Rp x -> F.fprintf fmt "@[Rp(%a)@]" (pp_list pp_r') x 

let () = 
  F.fprintf 
    F.std_formatter 
    "@[%a@]@."
    pp_or_ (Rp ( 
      ({r = {x = 1; y = "1"; z = "one"  } ; i = Some 10;l = [1.1 ; 2.1; 3.1] }) :: 
      ({r = {x = 2; y = "2"; z = "two"  } ; i = Some 20;l = [1.2 ; 2.2; 3.2] }) :: 
      ({r = {x = 3; y = "3"; z = "three"} ; i = None   ;l = [1.3 ; 2.3; 3.3] }) :: [] 
    ))
