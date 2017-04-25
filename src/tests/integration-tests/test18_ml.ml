module T  = Test18_types
module Pb = Test18_pb
module Pp = Test18_pp

let decode_ref_data () = T.({
  string_to_string = ("one", "two")::("three", "four")::[];
  string_to_int    = ("one", 1l)::("three", 3l)::[];
  int_to_int = (1, 2)::(3, 4)::[];
  int_to_message_value = (1l, {mv_field = "one"})::(2l, {mv_field = "two"})::[]; 
  int_to_enum_value = (1l, Ev_1)::(2l, Ev_2)::[]; 
  int_to_oneof_value = (1l, Ov_field1 "one")::(2l, Ov_field2 2l)::[];
})
  
let mode   = Test_util.parse_args ()

let sort_by_string_key l = 
  let cmp : string -> string -> int = compare in 
  let cmp (lhs, _) (rhs, _) = 
    cmp lhs rhs
  in 
  List.stable_sort cmp l

let sort_by_int_key l = 
  let cmp : int -> int -> int = compare in 
  let cmp (lhs, _) (rhs, _) = 
    cmp lhs rhs
  in 
  List.stable_sort cmp l

let sort_by_int32_key l = 
  let cmp : int32 -> int32 -> int = compare in 
  let cmp (lhs, _) (rhs, _) = 
    cmp lhs rhs
  in 
  List.stable_sort cmp l

let decode_maps decoder = 
  let maps = Pb.decode_maps decoder in 
  let {
    T.string_to_string; 
    T.string_to_int; 
    T.int_to_int; 
    T.int_to_message_value; 
    T.int_to_enum_value; 
    T.int_to_oneof_value; 
  } = maps in 
  {
    T.string_to_string = sort_by_string_key string_to_string;
    T.string_to_int = sort_by_string_key string_to_int;
    T.int_to_int = sort_by_int_key int_to_int; 
    T.int_to_message_value = sort_by_int32_key int_to_message_value; 
    T.int_to_enum_value = sort_by_int32_key int_to_enum_value; 
    T.int_to_oneof_value = sort_by_int32_key int_to_oneof_value;
  }

let () = 

  match mode with 
  | Test_util.Decode -> 
    Test_util.decode "test18.c2ml.data" decode_maps Pp.pp_maps (decode_ref_data  ()) 
  | Test_util.Encode -> 
    Test_util.encode "test18.ml2c.data" Pb.encode_maps (decode_ref_data ())
