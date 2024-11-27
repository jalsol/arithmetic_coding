let test_correctness () = 
  let input = Core.In_channel.read_all "./input.txt" in
  let encoded = Ac.Encode.encode input in
  let decoded = Ac.Decode.decode encoded in
  Alcotest.(check string) "same string" input decoded

let () =
  Alcotest.run "Test" [
    "Encoding to Decoding", [
      Alcotest.test_case "Correctness" `Quick test_correctness
    ]
  ]