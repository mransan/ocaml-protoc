open! Base
open! Base_quickcheck

module T = struct
  type t = unit [@@deriving equal, quickcheck, sexp_of]

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
