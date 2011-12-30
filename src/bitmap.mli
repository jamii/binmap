type t = Int32.t

type bigarray_elt = Bigarray.int32_elt
val bigarray_kind : (t, bigarray_elt) Bigarray.kind

val width : int

val zeroes : t
val ones : t

val get : t -> int -> bool
val set : t -> int -> bool -> t

val is_paired : t -> bool

val join : t -> t -> t (* !!! for safety unit join and is_paired !!! *)
val split : t -> t * t

val to_string : t -> string
val of_string : string -> t

val to_int : t -> int
val of_int : int -> t
