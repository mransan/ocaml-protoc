type t = Messages.error = { error: string }

include Roundtrip.S with type t := t
