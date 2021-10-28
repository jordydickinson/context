type index := Debruijn.index
type level := Debruijn.level

type 'a t =
  { path: 'a Name.t list
  ; name: 'a Name.t
  }

include Equatable.S1 with type 'a t := 'a t

type indexed = index t

type leveled = level t

val make: 'a Name.t list -> 'a Name.t -> 'a t