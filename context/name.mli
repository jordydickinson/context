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

  (** [extend id space] is [space] with an additional name identified by [id]
      at De Bruijn index 0. *)
  val extend: Ident.t -> t -> t

  (** [shrink id space] is [space] with the most-recently-added name identified
      by [id] removed, or @raise Not_found if no such name exists. *)
  val shrink: Ident.t -> t -> t

  (** [shrink_opt] is like {!val:shrink} but it returns [None] rather than
      raising an exception. *)
  val shrink_opt: Ident.t -> t -> t option

  (** [mem name space] is [true] if [name] is a member of [space]. *)
  val mem: _ name -> t -> bool
end