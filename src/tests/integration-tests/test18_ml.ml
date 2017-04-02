module T  = Test18_types
module Pb = Test18_pb
module Pp = Test18_pp

let decode_ref_data () = T.({
  string_to_string = List.rev @@ ("one", "two")::("three", "four")::[];
  string_to_int    = ("one", 1l)::("three", 3l)::[];
  int_to_int = List.rev @@ (1, 2)::(3, 4)::[];
  int_to_message_value = List.rev @@ (1l, {mv_field = "one"})::(2l, {mv_field = "two"})::[]; 
  int_to_enum_value = List.rev @@ (1l, Ev_1)::(2l, Ev_2)::[]; 
  int_to_oneof_value = List.rev @@ (1l, Ov_field1 "one")::(2l, Ov_field2 2l)::[];
})
  
let mode   = Test_util.parse_args ()

let () = 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode "test18.c2ml.data" Pb.decode_maps Pp.pp_maps (decode_ref_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test18.ml2c.data" Pb.encode_maps (decode_ref_data ())
