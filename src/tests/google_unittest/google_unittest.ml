module T = Unittest

let () =
  assert ((T.default_test_comment_injection_message ()).a = None);
  print_endline "Google unittest .... OK"
