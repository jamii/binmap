type t

val create : int -> t

val get : t -> int -> bool
val set : t -> int -> int -> bool -> unit

val to_string : t -> string
