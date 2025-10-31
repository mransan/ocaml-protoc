module T = Test16

let of_list e0 l =
  let f = Pbrt.Repeated_field.make e0 in
  List.iter (fun x -> Pbrt.Repeated_field.add x f) l;
  f

let decode_pb_ref_data () =
  let v = T.make_m ~f1:1l ~f2:2l ~f3:3l ~f4:(of_list 0l [ 1l; 2l ]) () in
  T.set_m_f3 v 4l;
  T.set_m_f4 v @@ of_list 0l [ 3l; 4l ];
  v

let mode = Test_util.parse_args ()

let () =
  match mode with
  | Test_util.Decode ->
    Test_util.decode "test16.c2ml.data" T.decode_pb_m T.pp_m
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test16.ml2c.data" T.encode_pb_m (decode_pb_ref_data ())
