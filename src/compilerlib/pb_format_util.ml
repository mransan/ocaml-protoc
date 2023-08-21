open Format


(* Not available in 4.03 *)
let pp_print_option ?(none = fun _ () -> ()) pp_v ppf = function
  | None -> none ppf ()
  | Some v -> pp_v ppf v

let pp_none ppf () = fprintf ppf "(None)"
