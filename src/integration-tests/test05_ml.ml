module Pc = Protobuf_codec 
module T  = Test05_pb 

let  max_int = 107374182
let  min_int = -107374182
let  inc     = 2500

let decode_ref_data () = 
  let rec loop l = function
    | i when i < max_int - (2 * inc) -> (
      loop (i::l) (i + inc)
    )
    | i -> (
      l 
    ) 
  in 
  { T.l = List.rev @@ loop [] (min_int) } 

let () = 

  let mode   = Test_util.parse_args () in 

  Printf.printf "min int: %i, max_int : %i, word size: %i\n" 
    (min_int)
    (max_int)
    (Sys.word_size);

  match mode with 
  | Test_util.Decode -> ( 
      let ref_data = decode_ref_data () in 
      Printf.printf "List size : %i\n%!" (List.length @@ ref_data.T.l);
      Test_util.decode ~noprint:() "test05.c2ml.data" T.decode_intlist T.string_of_intlist ref_data
  )
  | Test_util.Encode -> 
      Test_util.encode "test05.ml2c.data" T.encode_intlist (decode_ref_data ())
