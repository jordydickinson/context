type index := Debruijn.index
type level := Debruijn.level

type _ t =
| Indexed : Ident.t * index -> index t
| Leveled : Ident.t * level -> level t

include Equatable.S1 with type 'a t := 'a t

type indexed = index t
type leveled = level t

val indexed : Ident.t -> index -> indexed

val leveled : Ident.t -> level -> leveled

val ident : 'a t -> Ident.t

val debruijn : 'a t -> Debruijn.t

val drop_ident : 'a t -> 'a