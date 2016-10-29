module T  = Test05_pb 

let  max_int = 107374182l
let  min_int = -107374182l
let  inc     = 250l

let (+) = Int32.add
let (-) = Int32.sub
let ( * ) = Int32.mul

let decode_ref_data () = 
  let rec loop l = function
    | i when i < max_int - (2l* inc) -> (
      loop (i::l) (i + inc)
    )
    | _ -> l 
  in 
  { T.l = List.rev @@ loop [] (min_int) } 

let () = 

  let mode   = Test_util.parse_args () in 

  Printf.printf "min int: %li, max_int : %li, word size: %i\n" 
    (min_int)
    (max_int)
    (Sys.word_size);

  match mode with 
  | Test_util.Decode -> ( 
      let ref_data = decode_ref_data () in 
      Printf.printf "List size : %i\n%!" (List.length @@ ref_data.T.l);
      Test_util.decode ~noprint:() "test05.c2ml.data" T.decode_int_list T.pp_int_list ref_data
  )
  | Test_util.Encode -> 
      Test_util.encode "test05.ml2c.data" T.encode_int_list (decode_ref_data ())
