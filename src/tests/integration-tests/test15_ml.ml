module T  = Test15_pb

(** [fill_0_to_n n] create and fill a Pbrt.Repeated_field 
    with int32 values from [0; n].
 *)
let fill_0_to_n n = 
  let n = n + 1 in 
  let l = Pbrt.Repeated_field.make 0 in 
  let rec loop = function
    | i when i = n -> l 
    | i -> (
      Pbrt.Repeated_field.add i l;
      loop (i + 1) 
    )
  in 
  loop 0

let decode_ref_data () = T.({
  m1_l = [
    (
      {
        f = 123l;
        l = fill_0_to_n 100; 
      } 
    );
    {
      f = 456l;
      l = fill_0_to_n 5;
    }
  ];
})
  
let mode   = Test_util.parse_args ()

let () = 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode "test15.c2ml.data" T.decode_m2 T.pp_m2 (decode_ref_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test15.ml2c.data" T.encode_m2 (decode_ref_data ())
