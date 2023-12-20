type index := Local.index

(** A structure for associating data with qualified names. *)
type +'a t

(** [empty] contains no bindings *)
val empty: 'a t

(** [is_empty globals] is [true] if [globals] is empty and [false] otherwise. *)
val is_empty: 'a t -> bool

(** [singleton path id] is a globals structure with one binding at the given
    path/identifier. *)
val singleton: Ident.t list -> Ident.t -> 'a -> 'a t

(** [qualify path globals] is [globals] with all names qualified by [path]. *)
val qualify: Ident.t list -> 'a t -> 'a t

(** [add path id v globals] is [globals] with a binding of [v] for [id]
    qualified by [path]. This binding shadows any previous binding, but does not
    replace them. Instead, their De Bruijn indices are shifted upward. *)
val add: Ident.t list -> Ident.t -> 'a -> 'a t -> 'a t

(** [import path globals' globals] imports all the bindings of [globals'] into
    [globals], qualifying them by [path], as if each entry had been {!val:add}ed
    to [globals] in the same order as they were {!val:add}ed to [globals'].
    Previous bindings are shadowed but not replaced. *)
val import: Ident.t list -> 'a t -> 'a t -> 'a t

(** [remove path id globals] removes the last binding to be associated with [id]
    qualified by [path]. Any previous bindings have their indices shifted
    downward. If no such binding exists, [globals] is returned unchanged. *)
val remove: Ident.t list -> Ident.t -> 'a t -> 'a t

(** [find name globals] is the value bound to [name] in [globals] or @raise
    Not_found if no such binding exists. *)
val find: _ Global.t -> 'a t -> 'a

(** [find_opt] is like {!val:find}, but it returns [None] rather than raising
    an exception. *)
val find_opt: _ Global.t -> 'a t -> 'a option

(** [mem name globals] is [true] if there is a value bound to [name] in
    [globals] and [false] otherwise. *)
val mem: _ Global.t -> 'a t -> bool

(** [switch path globals] is a globals structure with all the bindings present
    at [path] or @raise Not_found if there are no bindings at [path]. *)
val switch: _ Name.t list -> 'a t -> 'a t

(** [switch_opt] is like {!val:switch} but it returns [None] rather than raising
    an exception. *)
val switch_opt: _ Name.t list -> 'a t -> 'a t option

(** [pool path id globals] is the pool of indices/levels associated with the
    identifier [id] qualified by [path] in [globals]. *)
val pool: _ Name.t list -> Ident.t -> 'a t -> Local.pool

(** [fix name globals] is the leveled equivalent of [name] in [globals] or
    @raise Not_found if [name] is not in [globals]. *)
val fix: _ Global.t -> 'a t -> Global.leveled

(** [fix_opt] is like {!val:fix} but it returns [None] rather than raising an
    exception. *)
val fix_opt: _ Global.t -> 'a t -> Global.leveled option

(** [iter f globals] applies [f] to every element of [globals] in unspecified
    order. *)
val iter: (Ident.t list -> Ident.t -> index -> 'a -> unit) -> 'a t -> unit

(** [fold f init globals] folds over the bindings in [globals] using the
    function [f] and [init] as the initial value for the accumulator in
    unspecified order. *)
val fold: (Ident.t list -> Ident.t -> index -> 'acc -> 'a -> 'acc) -> 'acc
  -> 'a t -> 'acc