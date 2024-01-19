module T = struct
  type t = Messages.error = { error: string } [@@deriving qcheck2]

  let quickcheck = Messages.quickcheck_error
end

include T

let%expect_test "roundtrip" =
  Roundtrip.run (module T);
  [%expect {||}];
  ()
