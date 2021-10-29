(** De Bruijn indices *)
type index

(** De Bruijn levels *)
type level

(** "Pools" of debruijn indices/levels, for conversion *)
type pool

(** A De Bruijn index or level *)
type t =
| Level of level
| Index of index

include Equatable.S with type t := t

module Index:
sig
  type t = index

  (** [succ i] is the successor index of [i]. Functionally equivalent to
      [of_int (to_int i + 1)]. *)
  val succ: t -> t

  (** Create an index from an integer with the same value *)
  val of_int: int -> t

  (** [to_int i] is the same as [(i :> int)]. *)
  val to_int: t -> int

  (** [of_level ~size l] an index from level [l], assuming a context of size
      [size]. *)
  val of_level: size:int -> level -> t

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end

module Level:
sig
  type t = level

  (** [zero] is the least possible level. *)
  val zero: t

  (** [of_index ~size i] creates a level from [i], assuming a context of size
      [size]. *)
  val of_index: size:int -> index -> t

  (** [succ l] is the successor level of [l]. *)
  val succ: t -> t

  include Equatable.S with type t := t
  include Comparable.S with type t := t
end

module Pool:
sig
  type db := t
  type t = pool

  (** The empty pool *)
  val empty: t

  (** Test whether a pool is empty *)
  val is_empty: t -> bool

  (** The number of De Bruijn indices/levels in this pool *)
  val size: t -> int

  (** [max_level pool] is the maximum De Bruijn level in [pool] or @raise
      Failure if [pool] is empty. *)
  val max_level: t -> level

  (** [max_level_opt] is like {!val:max_level} but it returns [None] rather than
      raising an exception. *)
  val max_level_opt: t -> level option

  (** [next_level pool] is one level past [max_level pool]. *)
  val next_level: t -> level

  (** Extend the pool with a new variable. *)
  val extend: t -> t

  (** Extend the pool with a new variable and return it as a level. *)
  val extend_level: t -> level * t

  (** [shrink pool] is [pool] with one variable removed or @raise Failure if
      [pool] is empty.
    *)
  val shrink: t -> t

  (** Test whether the given index is in the pool. *)
  val mem_index: index -> t -> bool

  (** Test whether the given level is in the pool. *)
  val mem_level: level -> t -> bool

  (** Test whether the given index or level is in the pool. *)
  val mem: db -> t -> bool

  (** [index_to_level pool i] is the level in [pool] corresponding to index [i]. *)
  val index_to_level: pool -> index -> level

  (** [level_to_index pool l] is the index in [pool] corresponding to level [l]. *)
  val level_to_index: pool -> level -> index

  (** [to_level pool db] is the level in [pool] corresponding to index/level
      [db].
    *)
  val to_level: pool -> db -> level

  (** [to_index pool db] is the index in [pool] corresponding to index/level
      [db].
    *)
  val to_index: pool -> db -> index
end