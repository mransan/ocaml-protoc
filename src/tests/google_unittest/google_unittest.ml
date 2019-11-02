module T = Unittest_types

let () = 
  assert({T.a = Some "*/ <- Neither should this."} = T.default_test_comment_injection_message ());
  print_endline "Google unittest .... OK"

