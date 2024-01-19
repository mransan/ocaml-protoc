module T = struct
  type t = Messages.unit_or_error =
    | Unit
    | Error of Error_.t
  [@@deriving qcheck2]

  let pp = Messages.pp_unit_or_error
  let equal = Messages.equal_unit_or_error
  let encode_pb = Messages.encode_pb_unit_or_error
  let decode_pb = Messages.decode_pb_unit_or_error
  let encode_json = Messages.encode_json_unit_or_error
  let decode_json = Messages.decode_json_unit_or_error
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
