type t = Messages.unit_or_error =
  | Unit
  | Error of Error_.t

include Roundtrip.S with type t := t
