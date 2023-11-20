type index := Local.index
type level := Local.level

(** Local or global variables. *)
type (_, _) t =
| Local : 'l Local.t -> ('l, _) t
| Global : 'g Global.t -> (_, 'g) t

include Equatable.S2 with type ('l, 'g) t := ('l, 'g) t

(** Indexed variables *)
type indexed = (index, index) t

(** Indexed locals, leveled globals *)
type indexed_leveled = (index, level) t

(** Leveled variables *)
type leveled = (level, level) t

(** Leveled locals, indexed globals *)
type leveled_indexed = (level, index) t

val compare: ('l, 'g) t -> ('l, 'g) t -> int

module Indexed: sig
  type t = indexed

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end

module Indexed_leveled: sig
  type t = indexed_leveled

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end

module Leveled: sig
  type t = leveled

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end

module Leveled_indexed: sig
  type t = leveled_indexed

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end