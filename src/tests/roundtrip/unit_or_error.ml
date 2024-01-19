module T = struct
  type t = Messages.unit_or_error =
    | Unit
    | Error of Error_.t
  [@@deriving qcheck2]

  let quickcheck = Messages.quickcheck_unit_or_error
end

include T

let%expect_test "roundtrip" =
  Roundtrip.run (module T);
  [%expect
    {|
    QCheck2.Test.check_cell failed
    input: Unit
    error: Pbrt.Decoder.Failure(Malformed_variant("unit_or_error")) |}];
  ()
