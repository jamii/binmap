type t

val create : int -> t

val get : t -> int -> bool
val set : t -> int -> bool -> unit

val to_string : t -> string
val of_string : string -> t
