type index := Local.index
type level := Local.level
type pool := Local.pool

(** Essentially a glorified list type, mostly for storing data associated with
    local variables in a way that makes handling of DeBruijn levels and indices
    much easier to reason about. *)
type +'a t = private { elts: 'a list; pool: pool }

(** [empty] contains no entries. *)
val empty : 'a t

(** [is_empty locals] is [true] if [locals] is empty and [false] otherwise. *)
val is_empty : 'a t -> bool

(** [elts locals] is a list of the elements in [locals], from
    most-recently-bound to least-recently-bound. *)
val elts: 'a t -> 'a list

(** [pool locals] is the pool of variables associated with [locals]. *)
val pool: 'a t -> pool

(** [add v locals] is [locals] with [v] added at De Bruijn index 0, and all
    other entries shifted upward by one. *)
val add: 'a -> 'a t -> 'a t

(** [singleton v] is functionally equivalent to [add v empty]. *)
val singleton: 'a -> 'a t

(** [top xs] is the value associated with the most-recently-bound variable;
    i.e., at De Bruijn index 0.

    @raise [Failure] if [xs] is empty. *)
val top: 'a t -> 'a

(** Like [top] but returns [None] rather than raising an exception. *)
val top_opt: 'a t -> 'a option

(** [pop xs] is a pair consisting of the most-recently-bound variable and [xs]
    without the most-recently-bound variable or @raise Failure if [xs] is empty. *)
val pop: 'a t -> 'a * 'a t

(** [pop_opt] is like {!val:pop} but returns [None] rather than raising an
    exception. *)
val pop_opt: 'a t -> ('a * 'a t) option

(** [drop xs] is [xs] without [top xs] or @raise Failure if [xs] is empty. *)
val drop: 'a t -> 'a t

(** [drop_opt] is like {!val:drop}, but returns [None] rather than raising an
    exception. *)
val drop_opt: 'a t -> 'a t option

(** [get db locals] is the value bound to De Bruijn index/level [db] or @raise
    Failure if there is no entry associated with [db]. *)
val get: _ Local.t -> 'a t -> 'a

(** [get_opt] is like {!val:get} but it returns [None] rather than raising an
    exception. *)
val get_opt: _ Local.t -> 'a t -> 'a option

(** [mem db locals] is [true] if [db] is associated with some binding in
    [locals] and [false] otherwise. *)
val mem: _ Local.t -> 'a t -> bool

(** [ith i locals] is the binding associated with De Bruijn index [i], or
    @raise Failure if no such binding exists. *)
val ith: index -> 'a t -> 'a

(** [lth] is like {!val:ith}, but it takes a De Bruijn level rather than an
    index. *)
val lth: level -> 'a t -> 'a

(** [ith_opt] is like {!val:ith}, but returns [None] rather than raising an
    exception. *)
val ith_opt: index -> 'a t -> 'a option

(** [lth_opt] is like {!val:lth}, but returns [None] rather than raising an
    exception. *)
val lth_opt: level -> 'a t -> 'a option

(** [fix local locals] is the leveled equivalent of [local] in [locals] or
    @raise Failure if [local] is not in [locals]. *)
val fix: 'l Local.t -> 'a t -> Local.leveled

(** [fix_opt] is like {!val:fix}, but returns [None] rather than raising an
    exception. *)
val fix_opt: 'l Local.t -> 'a t -> Local.leveled option

val iter: ('a -> unit) -> 'a t -> unit

val iteri: (index -> 'a -> unit) -> 'a t -> unit

val map: ('a -> 'b) -> 'a t -> 'b t

(** [combine locals1 locals2] combines the bindings of [locals1] and [locals2]
    such that if local [l] is bound to [a] in [locals1] and [b] in [locals2]
    then [combine locals1 locals2] binds [a, b] to [l].
    
    @raise [Invalid_argument] if there exists some local [l] bound in [locals1]
      (or [locals2]) which is not bound in [locals2] (or [locals1]). *)
val combine: 'a t -> 'b t -> ('a * 'b) t

val iter2: ('a -> 'b -> unit) -> 'a t -> 'b t -> unit

val iteri2: (index -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit

val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val find: ('a -> bool) -> 'a t -> 'a

val find_opt: ('a -> bool) -> 'a t -> 'a option

val findi: (index -> 'a -> bool) -> 'a t -> index * 'a

val findi_opt: (index -> 'a -> bool) -> 'a t -> (index * 'a) option

val findl: (level -> 'a -> bool) -> 'a t -> level * 'a

val findl_opt: (level -> 'a -> bool) -> 'a t -> (level * 'a) option

val index: ('a -> bool) -> 'a t -> index

val index_opt: ('a -> bool) -> 'a t -> index option

val level: ('a -> bool) -> 'a t -> level

val level_opt: ('a -> bool) -> 'a t -> level option