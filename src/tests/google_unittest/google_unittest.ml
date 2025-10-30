module T = Unittest

let () =
  assert ({ T.a = None } = T.default_test_comment_injection_message);
  print_endline "Google unittest .... OK"
