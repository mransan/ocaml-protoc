module T = struct
  type t = unit [@@deriving qcheck2]

  let pp = Messages.pp_empty
  let equal = Messages.equal_empty
  let encode_pb = Messages.encode_pb_empty
  let decode_pb = Messages.decode_pb_empty
  let encode_json = Messages.encode_json_empty
  let decode_json = Messages.decode_json_empty
end

include T

let%expect_test "roundtrip" =
  Roundtrip.run (module T);
  [%expect {||}];
  ()
