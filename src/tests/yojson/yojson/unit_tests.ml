open! Pbrt_yojson

let () =
  assert ("abc" = string (`String "abc") "r" "f");
  assert (1.2 = float (`Float 1.2) "r" "f");
  assert (1.2 = float (`String "1.2") "r" "f");
  assert (1.0 = float (`Int 1) "r" "f");
  assert (123l = int32 (`Int 123) "r" "f");
  assert (123l = int32 (`String "123") "r" "f");
  assert (123l = int32 (`Float 123.0) "r" "f");
  assert (123L = int64 (`Int 123) "r" "f");
  assert (123L = int64 (`String "123") "r" "f");
  assert (123L = int64 (`Float 123.0) "r" "f");
  assert (true = bool (`Bool true) "r" "f");
  assert (false = bool (`Bool false) "r" "f");
  ()

