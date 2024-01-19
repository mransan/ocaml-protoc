module T = struct
  type t = Messages.person = {
    name: string;
    id: int32;
    email: string;
    phone: string list;
  }
  [@@deriving qcheck2]

  let quickcheck = Messages.quickcheck_person
end

include T

let%expect_test "roundtrip" =
  Roundtrip.run (module T);
  [%expect {||}];
  ()
