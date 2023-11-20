type index := Local.index
type level := Local.level
type pool := Local.pool

type _ t =
| Indexed : Ident.t * index -> index t
| Leveled : Ident.t * level -> level t

include Equatable.S1 with type 'a t := 'a t

type indexed = index t
type leveled = level t

type space

val indexed : Ident.t -> index -> indexed

val leveled : Ident.t -> level -> leveled

val ident : 'a t -> Ident.t

val to_local : 'a t -> 'a Local.t

val drop_ident : 'a t -> 'a

val compare : 'a t -> 'a t -> int

module Indexed: sig
  type t = indexed

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end

module Leveled: sig
  type t = leveled

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end

module Space: sig
  type 'a name := 'a t
  type t = space

  (** [empty] is the namespace with no names. *)
  val empty: t

  (** [is_empty space] is [true] if [space] is empty and [false] otherwise. *)
  val is_empty: t -> bool

  (** [subspace space1 space2] is [true] if all names in [space1] are also in
      [space2]. *)
  val subspace: t -> t -> bool

  (** [union space1 space2] is a namespace containing all the names in both
      [space1] and [space2]. *)
  val union: t -> t -> t

  (** [pool id space] is the pool of indices/levels associated with [id] in
      [space]. If [space] contains no names identified by [id], this pool is
      empty. *)
  val pool: Ident.t -> t -> pool

  (** [add id space] is [space] with an additional name identified by [id]. If
      a name with this [id] already exists, it is shadowed. *)
  val add: Ident.t -> t -> t

  (** [remove id space] is [space] with the most-recently-{!val:add}ed name with
      identifier [id] removed. If no such name existed, [space] is returned
      unchanged.  *)
  val remove: Ident.t -> t -> t

  (** [mem name space] is [true] if [name] is a member of [space]. *)
  val mem: _ name -> t -> bool
end