

module T = Unittest_pb


let () = 
  assert({T.a = Some "*/ <- Neither should this."} = T.default_test_comment_injection_message ()) 
