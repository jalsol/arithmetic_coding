let rec encode_helper (model: Model.t) (low: Q.t) (high: Q.t) (input: char list) : Q.t =
  match input with
  | [] -> Q.(low + (high - low) / of_int 2)
  | ch :: tail -> (
    let range = Q.(high - low) in
    let (lower, upper) = Model.get_range model ch in
    let next_low = Q.(low + range * lower) in
    let next_high = Q.(low + range * upper) in
    let _ = Model.add_symbol model ch in
    encode_helper model next_low next_high tail
  )

let encode input =
  let input_list = String.to_seq input |> List.of_seq in
  let model = Model.make () in
  encode_helper model (Q.of_int 0) (Q.of_int 1) input_list
