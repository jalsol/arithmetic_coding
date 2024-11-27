type t

val make : unit -> t
val total : t -> int
val add_symbol : t -> char -> unit
val get_range : t -> char -> Q.t * Q.t
val get_symbol : t -> Q.t -> char