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
val find: _ Qname.t -> 'a t -> 'a

(** [find_opt] is like {!val:find}, but it returns [None] rather than raising
    an exception. *)
val find_opt: _ Qname.t -> 'a t -> 'a option

(** [mem name globals] is [true] if there is a value bound to [name] in
    [globals] and [false] otherwise. *)
val mem: _ Qname.t -> 'a t -> bool

(** [switch path globals] is a globals structure with all the bindings present
    at [path] or @raise Not_found if there are no bindings at [path]. *)
val switch: _ Name.t list -> 'a t -> 'a t

(** [switch_opt] is like {!val:switch} but it returns [None] rather than raising
    an exception. *)
val switch_opt: _ Name.t list -> 'a t -> 'a t option