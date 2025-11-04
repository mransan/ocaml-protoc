module T = Unittest

let () =
  assert (
    not
    @@ T.test_comment_injection_message_has_a
         (T.default_test_comment_injection_message ()));
  print_endline "Google unittest .... OK"
