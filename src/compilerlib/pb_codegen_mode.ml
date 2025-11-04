type t =
  [ `Normal
  | `Encode_only
  | `Decode_only
  ]

let to_string = function
  | `Normal -> "normal"
  | `Encode_only -> "encode_only"
  | `Decode_only -> "decode_only"

let do_encode = function
  | `Normal | `Encode_only -> true
  | `Decode_only -> false

let do_decode = function
  | `Normal | `Decode_only -> true
  | `Encode_only -> false
