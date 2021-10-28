module Level := Debruijn.Level
type pool := Debruijn.pool

(** A structure for associating arbitrary data with scopes. Unlike
    {!type:Locals.t}, this type supports random-access updates of its
    contents. *)
type +'a t = private
  { data: 'a Level.Map.t
  ; pool: pool
  }

(** [root] is the root scope. *)
val root: 'a t

(** [is_root scope] is [true] if [scope] is the root scope and [false] otherwise. *)
val is_root: 'a t -> bool

(** [enter ?data scope] is a new, nested scope within [scope]. If [data] is
    provided it is associated with this scope and accessible at De Bruijn index
    0. *)
val enter: ?data:'a -> 'a t -> 'a t

(** [exit scope] is the parent scope of [scope] or @raise Failure if [scope] is
    the root scope. *)
val exit: 'a t -> 'a t

(** [exit_opt] is like {!val:exit}, but it returns [None] rather than raising an
    exception. *)
val exit_opt: 'a t -> 'a t option

(** [find i scope] finds the data associated with the scope at level/index [i]
    in [scope] or @raise Not_found if no such data exists. *)
val find: Debruijn.t -> 'a t -> 'a

(** [find_opt] is like {!val:find} but it returns [None] rather than raising an
    exception. *)
val find_opt: Debruijn.t -> 'a t -> 'a option

(** [remove i scope] is [scope] without the data associated with level/index [i]
    or just [scope] if no such data exists. *)
val remove: Debruijn.t -> 'a t -> 'a t

(** [update i f scope] updates the data associated with index/level [i] in
    [scope] according to the function [f]. In particular, if no data is
    associated with [i], then [f None] will be called; otherwise [f (Some v)]
    where [v] is the data in question. If [f] returns [None], any association
    will be removed; otherwise, if [f] return [Some v'], [v'] will be associated
    with [i]. Note that if [i] does not refer to an open scope, [scope] is
    returned unchanged and [f] is never called. *)
val update: Debruijn.t -> ('a option -> 'a option) -> 'a t -> 'a t