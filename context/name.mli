type index := Local.index
type level := Local.level

type _ t =
| Indexed : Ident.t * index -> index t
| Leveled : Ident.t * level -> level t

include Equatable.S1 with type 'a t := 'a t

type indexed = index t
type leveled = level t

val indexed : Ident.t -> index -> indexed

val leveled : Ident.t -> level -> leveled

val ident : 'a t -> Ident.t

val to_local : 'a t -> 'a Local.t

val drop_ident : 'a t -> 'a