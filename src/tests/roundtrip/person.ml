open! Base
open! Base_quickcheck

module T = struct
  type t = Messages.person = {
    name: string;
    id: int32;
    email: string;
    phone: string list;
  }
  [@@deriving equal, qcheck2, quickcheck, sexp_of]

  let encode_pb = Messages.encode_pb_person
  let decode_pb = Messages.decode_pb_person
  let encode_json = Messages.encode_json_person
  let decode_json = Messages.decode_json_person
end

include T

let%expect_test "roundtrip" =
  Roundtrip.run (module T);
  [%expect {||}];
  ()
