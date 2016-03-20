module T  = Test16_pb

let of_list e0 l = 
  let f = Pbrt.Repeated_field.make e0 in 
  List.iter (fun x -> Pbrt.Repeated_field.add x f) l; 
  f  

let decode_ref_data () = 
  let v = T.({
    f1 = 1l;
    f2 = 2l;
    f3 = 3l;
    f4 = of_list 0l [ 1l; 2l; ];
  }) in 
  v.T.f3 <- 4l;
  v.T.f4 <- of_list 0l [ 3l; 4l; ]; 
  v 
  
let mode   = Test_util.parse_args ()

let () = 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode "test16.c2ml.data" T.decode_m T.pp_m (decode_ref_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test16.ml2c.data" T.encode_m (decode_ref_data ())
