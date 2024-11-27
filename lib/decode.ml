open Q

let rec decode_helper (low: Q.t) (high: Q.t) (input: Q.t) : char list =
  let range = high - low in
  let symbol = Model.get_symbol ((input - low) / range) in
  if symbol == '%' then (* '%' is EOF *)
    [symbol]
  else
    let (lower, upper) = Model.get_range symbol in
    let next_low = low + range * lower in
    let next_high = low + range * upper in
    symbol :: decode_helper next_low next_high input

let decode (input: Q.t) : string =
  let char_list = decode_helper (Q.of_int 0) (Q.of_int 1) input in
  String.concat "" (List.map (String.make 1) char_list)
