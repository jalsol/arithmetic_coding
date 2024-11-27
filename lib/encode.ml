let rec encode_helper (low: Q.t) (high: Q.t) (input: char list) : Q.t =
  let open Q in
  match input with
  | [] -> low + (high - low) / Q.of_int 2
  | ch :: tail -> (
    let range = high - low in
    let (lower, upper) = Model.get_range ch in
    let next_low = low + range * lower in
    let next_high = low + range * upper in
    encode_helper next_low next_high tail
  )

let encode input =
  let input_list = String.to_seq input |> List.of_seq in
  encode_helper (Q.of_int 0) (Q.of_int 1) input_list
