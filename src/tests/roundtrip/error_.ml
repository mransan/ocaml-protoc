open! Base
open! Base_quickcheck

module T = struct
  type t = Messages.error = { error: string }
  [@@deriving equal, quickcheck, sexp_of]

  let encode_pb = Messages.encode_pb_error
  let decode_pb = Messages.decode_pb_error
  let encode_json = Messages.encode_json_error
  let decode_json = Messages.decode_json_error
end

include T

let%expect_test "roundtrip" =
  Roundtrip.run (module T);
  [%expect {||}];
  ()
