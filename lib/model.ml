type t = { freq : int array }

let num_chars = 256

let make () =
  { freq = Array.init (num_chars + 1) (fun i -> i) }

let total (model: t) : int = model.freq.(num_chars)

let add_symbol (model: t) (ch: char) =
  let index = int_of_char ch in
  for i = index + 1 to num_chars do
    model.freq.(i) <- model.freq.(i) + 1
  done

let get_range (model: t) (ch: char) : (Q.t * Q.t) =
  let order = int_of_char ch in
  let freq  = model.freq in
  let lower = Q.of_int freq.(order) in
  let upper = Q.of_int freq.(order + 1) in
  let total = Q.of_int (total model) in
  Q.(lower / total, upper / total)

let rec get_symbol' (model: t) (input: Q.t) (left: int) (right: int) (ans: int) : int =
  if left > right then
    ans
  else
    let mid = left + (right - left) / 2 in
    let total    = Q.of_int (total model) in
    let freq     = Q.of_int model.freq.(mid + 1) in
    if Q.(input * total < freq) then
      get_symbol' model input left (mid - 1) mid
    else
      get_symbol' model input (mid + 1) right ans

let get_symbol (model: t) (input: Q.t): char =
  let ord = get_symbol' model input 0 num_chars 0 in
  char_of_int ord