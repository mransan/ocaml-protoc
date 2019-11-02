let () = 
  let s = "message /* message M {} */ message" in 
  Test_util.loop (Lexing.from_string s)
