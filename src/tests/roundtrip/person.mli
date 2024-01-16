type t = Messages.person = {
  name: string;
  id: int32;
  email: string;
  phone: string list;
}

include Roundtrip.S with type t := t
