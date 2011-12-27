type t = Int64

val width : int

val zeroes : t
val ones : t

val get : t -> int -> bool
val set : t -> int -> bool -> t

val is_paired : t -> bool

val join : t -> t -> t
val unjoin : t -> t * t

val split : t -> Int32.t * Int32.t

val to_string : t -> string
val of_string : string -> t
