let text =
  let rec read_lines () =
    try
      let line = read_line () in
      line ^ "\n" ^ read_lines ()
    with
    | End_of_file -> ""
  in read_lines ()

let () =
  let encoded = Ac.Encode.encode text in
  Printf.printf "%.60f\n" (Q.to_float encoded);
  let decoded = Ac.Decode.decode encoded in
  print_endline decoded

