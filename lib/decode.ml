let rec decode_helper (model: Model.t) (low: Q.t) (high: Q.t) (input: Q.t) : char list =
  let range = Q.(high - low) in
  let symbol = Model.get_symbol model Q.((input - low) / range) in
  if symbol == '%' then (* '%' is EOF *)
    [symbol]
  else
    let (lower, upper) = Model.get_range model symbol in
    let next_low = Q.(low + range * lower) in
    let next_high = Q.(low + range * upper) in
    let _ = Model.add_symbol model symbol in
    symbol :: decode_helper model next_low next_high input

let decode (input: Q.t) : string =
  let model = Model.make () in
  let char_list = decode_helper model (Q.of_int 0) (Q.of_int 1) input in
  String.concat "" (List.map (String.make 1) char_list)
