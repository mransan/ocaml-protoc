module T = struct
  type t = Messages.person = {
    name: string;
    id: int32;
    email: string;
    phone: string list;
  }
  [@@deriving qcheck2]

  let pp = Messages.pp_person
  let equal = Messages.equal_person
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
