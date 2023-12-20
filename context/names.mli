type index := Local.index
type level := Local.level

(** A multimap-like structure for associating data with names and managing their
    levels and indices. *)
type +'a t

(** A change in bindings, used by {!val:change}. *)
type 'a change =
| Add of 'a
  (** Add the given value, shadowing (but not removing) the previous
      binding. *)
| Remove
  (** Remove the most recent binding. If none exists, do nothing. *)
| Replace of 'a
  (** Replace the most recent binding with the provided value. If none exists,
      create a new binding with the given value, as in {!constr:Add}. *)

(** [empty] contains no bindings. *)
val empty : 'a t

(** [is_empty names] is [true] if [names] is empty and [false] otherwise. *)
val is_empty : 'a t -> bool

(** [add id v names] adds the binding of [v] to [id], shadowing any previous
    binding to [id]. Any previous bindings can still be accessed at the next
    De Bruijn index, or restored with [remove]. *)
val add: Ident.t -> 'a -> 'a t -> 'a t

(** [singleton id v] is a structure with only one binding of [v] to [id]. *)
val singleton: Ident.t -> 'a -> 'a t

(** [remove id names] removes the most recent binding to [id]. If [id] was
    shadowing previous bindings, those are retained and their De Bruijn indices
    are shifted down by 1. If there are no bindings for [id], [names] is
    returned unchanged. *)
val remove: Ident.t -> 'a t -> 'a t

(** [replace id v names] is functionally equivalent to
    [add id v (remove id names)]. *)
val replace: Ident.t -> 'a -> 'a t -> 'a t

(** [update id f names] looks up the most recent binding for [id]. If a binding
    [v] is found, [f (Some v)] is called; otherwise [f None] is called. In
    either case, the entry is updated according to the result of [f]. If [f]
    returns [None], the binding is removed, as if [remove id names] were called.
    Otherwise if [f] returns [Some v'], [v'] is used as the new binding, as if
    [replace id v' names] were called. *)
val update: Ident.t -> ('a option -> 'a option) -> 'a t -> 'a t

(** [change id f names] is similar to [update id f names], but the function [f]
    is able to specify more precise options regarding how to update the binding.
    See {!type:change} for more details. *)
val change: Ident.t -> ('a option -> 'a change) -> 'a t -> 'a t

(** [import names' names] imports the bindings of [names'] into [names]. In
    particular, bindings of [names'] shadow those of [names], but do not
    replace them, as if they were all individually {!val:add}ed to [names] in
    the same order they were {!val:add}ed to [names']. *)
val import: 'a t -> 'a t -> 'a t

(** [find name names] is the value bound to [name] in [names]. Note that if
    [name] is De-Bruijn-indexed, then De Bruijn index 0 refers to the most
    recent binding to [name]'s identifier, index 1 to the next-most, and so on.
    As one would expect, De Bruijn level 0 refers to the first binding.
    
    @raise Not_found if no such binding exists. *)
val find: _ Name.t -> 'a t -> 'a

(** [find_opt] is like {!val:find}, but it returns [None] rather than raising an
    exception. *)
val find_opt: _ Name.t -> 'a t -> 'a option

(** [mem name names] is [true] if a binding to [name] exists in [names], and
    [false] otherwise. *)
val mem: _ Name.t -> 'a t -> bool

(** [iter f names] iterates over the bindings in [names], applying [f] to each
    binding. Identifiers are traversed in lexicographically increasing order,
    and for each identifier, bindings are traversed from most-recently to least-
    recently bound. Tail-recursive. *)
val iter: (Ident.t -> index -> 'a -> unit) -> 'a t -> unit

(** [fold_left f init names] folds over the bindings in [names] using the
    function [f] and [init] as the initial value for the accumulator.
    Identifiers are traversed in lexographically increasing order, and for
    each identifier, bindings are traversed from most-recently to least-recently
    bound. Tail-recursive. *)
val fold_left: (Ident.t -> index -> 'acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

(** [fold_right] is like [fold_left], but for each identifier, bindings are
    traversed from least-recently to most-recently bound. Folding over
    identifiers is tail-recursive, but folding over the bindings of each
    identifier is not. *)
val fold_right: (Ident.t -> level -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

(** [pool id names] is the pool of De Bruijn indices/levels associated with the
    identifier [id] in [names]. *)
val pool: Ident.t -> 'a t -> Local.pool

(** [fix name names] is the leveled equivalent of [name] in [names] or @raise
    Not_found if [name] is not in [names]. *)
val fix: _ Name.t -> 'a t -> Name.leveled

(** [fix_opt] is like {!val:fix} but it returns [None] rather than raising an
    exception. *)
val fix_opt: _ Name.t -> 'a t -> Name.leveled option