(** De Bruijn indices *)
type index = private Index of int

(** De Bruijn levels *)
type level = private Level of int

(** "Pools" of debruijn indices/levels, for conversion *)
type pool = private Pool of int

(** A De Bruijn index or level *)
type _ t =
| Indexed : index -> index t
| Leveled : level -> level t

type indexed = index t
type leveled = level t

val compare : 'a t -> 'a t -> int
val hash: 'a t -> int

include Equatable.S1 with type 'a t := 'a t

module Index:
sig
  type t = index

  (** [succ i] is the successor index of [i]. Functionally equivalent to
      [of_int (to_int i + 1)]. *)
  val succ: t -> t

  (** [pred i] is the predecessor index of [i] or @raise Invalid_arg if [i] has
      no predecessor. *)
  val pred: t -> t

  (** Create an index from an integer with the same value *)
  val of_int: int -> t

  (** [to_int i] is a non-negative integer with the same value as [i]. *)
  val to_int: t -> int

  (** [shift ?min ~amt i] is [i] shifted upwards by [amt] iff it is greater than
      or equal to [min]. If [min] is not provided it defaults to [0]. *)
  val shift: ?min:t -> amt:int -> t -> t

  include Equatable.S with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Level:
sig
  type t = level

  (** [zero] is the least possible level. *)
  val zero: t

  (** [succ l] is the successor level of [l]. *)
  val succ: t -> t

  include Equatable.S with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Pool:
sig
  type 'a db := 'a t
  type t = pool

  (** The empty pool *)
  val empty: t

  (** Test whether a pool is empty *)
  val is_empty: t -> bool

  (** [to_index_set pool] is the set of all indices in [pool]. *)
  val to_index_set: t -> Index.Set.t

  (** [subpool pool1 pool2] is [true] if [pool1] contains all the variables
      present in [pool2] and [false] otherwise. *)
  val subpool: t -> t -> bool

  (** [union pool1 pool2] is a pool containing the variables of both [pool1]
      and [pool2]. *)
  val union: t -> t -> t

  (** The number of De Bruijn indices/levels in this pool *)
  val size: t -> int

  (** [of_level l] is a pool which has exactly as many variables are necessary
      for [l] to be in the pool. *)
  val of_level: level -> t

  (** [max_level pool] is the maximum De Bruijn level in [pool] or @raise
      Failure if [pool] is empty. *)
  val max_level: t -> level

  (** [max_level_opt] is like {!val:max_level} but it returns [None] rather than
      raising an exception. *)
  val max_level_opt: t -> level option

  (** [next_level pool] is one level past [max_level pool]. *)
  val next_level: t -> level

  (** [fill ?amt pool] is [pool] extended with [amt] new variables. If [amt] is
      not provided, it defaults to [1].

      @raise Invalid_arg if [amt] is negative.
   *)
  val fill: ?amt:int -> t -> t

  (** [drain ?amt pool] is [pool] with [amt] fewer variables. If [amt] is not
      provided, it defaults to [1].

      @raise Invalid_arg if [amt] is negative.
      @raise Failure if [pool] has fewer than [amt] variables.
    *)
  val drain: ?amt:int -> t -> t

  (** Test whether the given index is in the pool. *)
  val mem_index: index -> t -> bool

  (** Test whether the given level is in the pool. *)
  val mem_level: level -> t -> bool

  (** Test whether the given index or level is in the pool. *)
  val mem: _ db -> t -> bool

  (** [index_of_level pool l] is the index corresponding to [l] in [pool] or
      @raise Invalid_arg if [l] is not in [pool]. *)
  val index_of_level: t -> level -> index

  (** [index_of_level_opt] is like {!val:index_of_level} but it returns [None]
      rather than raising an exception. *)
  val index_of_level_opt: t -> level -> index option

  (** [level_of_index pool i] is the level corresponding to [i] in [pool] or
      @raise Invalid_arg if [i] is not in [pool]. *)
  val level_of_index: t -> index -> level

  (** [level_of_index_opt] is like {!val:level_of_index} but it returns [None]
      rather than raising an exception. *)
  val level_of_index_opt: t -> index -> level option

  (** [to_index] is like {!val:index_of_level}, but it reduces to the identity
      function when provided an index. *)
  val to_index: t -> _ db -> index

  (** [to_index_opt] is like {!val:to_index} but it returns [None] rather than
      raising an exception. *)
  val to_index_opt: t -> _ db -> index option

  (** [to_level] is like {!val:level_of_index}, but it reduces to the identity
      function when provided a level. *)
  val to_level: t -> _ db -> level

  (** [to_level_opt] is like {!val:to_level} but it returns [None] rather than
      raising an exception. *)
  val to_level_opt: t -> _ db -> level option

  include Equatable.S with type t := t
end