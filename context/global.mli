type index := Local.index
type level := Local.level

type 'a t =
  { path: 'a Name.t list
  ; name: 'a Name.t
  }

include Equatable.S1 with type 'a t := 'a t

type indexed = index t

type leveled = level t

val make: 'a Name.t list -> 'a Name.t -> 'a t

val compare: 'a t -> 'a t -> int

val hash: 'a t -> int

module Indexed: sig
  type t = indexed

  include Equatable.S with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Leveled: sig
  type t = leveled

  include Equatable.S with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end