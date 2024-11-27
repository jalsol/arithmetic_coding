open Q

let num_chars = 256
let step = Q.of_int 1 / Q.of_int num_chars

let get_range (ch: char): (Q.t * Q.t) =
  let order = int_of_char ch in
  let lower = Q.of_int order * step in
  let upper = lower + step in
  (lower, upper)

let get_symbol (input: Q.t): char =
  let ord = input / step in
  char_of_int (Q.to_int ord)