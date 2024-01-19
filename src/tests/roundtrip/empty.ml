module T = struct
  type t = unit [@@deriving qcheck2]

  let quickcheck = Messages.quickcheck_empty
end

include T

let%expect_test "roundtrip" =
  Roundtrip.run (module T);
  [%expect {||}];
  ()
