(** A structure for associating data with qualified names. Unlike
    {!type:Globals.t}, paths and identifiers are not associated with De Bruijn
    indices or levels. For this reason, all qualified names must be unique and
    cannot be shadowed. *)
type +'a t

(** [empty] contains no bindings *)
val empty: 'a t

(** [is_empty globals] is [true] if [globals] is empty and [false] otherwise. *)
val is_empty: 'a t -> bool

(** [singleton qname v] is a globals structure with one binding [v] at the
    given global [qname]. *)
val singleton: Unique_global.t -> 'a -> 'a t

(** [qualify path globals] is [globals] with all names qualified by [path]. *)
val qualify: Ident.t list -> 'a t -> 'a t

(** [add qname v globals] is [globals] with a binding of [v] for [qname]. This
    binding replaces any previous binding. *)
val add: Unique_global.t -> 'a -> 'a t -> 'a t

(** [includ path globals' globals] includes all the bindings of [globals'] into
    [globals], qualifying them by [path], as if each entry had been {!val:add}ed
    to [globals] in the same order as they were {!val:add}ed to [globals'].
    Previous bindings are replaced. This is distinct from an import in the sense
    that all previous bindings at [path] are merged with all new bindings in
    [globals']. @see {!val:import} for more details. *)
val includ: Ident.t list -> 'a t -> 'a t -> 'a t

(** [import qname globals' globals] imports all the bindings of [globals'] into
    [globals], by assigning all of [globals'] bindings to the namespace [qname].
    Where [includ (qname.path :: qname.ident) globals' globals] merges the
    bindings of [globals'] with those already present at the prefix [qname.path
    :: qname.ident], [import] instead replaces all bindings at this path, such
    that none of them are accessible. This operation is similar to defining
    a module [module M = ...] in OCaml which shadows some module [M] which was
    inherited from a module [open]. Of particular note, the entire old module
    [M] is replaced, along with all its bindings. By contrast, an [includ] would
    merge the bindings of the new [M] and the old [M]. *)
val import: Unique_global.t -> 'a t -> 'a t -> 'a t

(** [merge f globals globals'] computes a global environment whose keys are a
    subset of the union of the keys of [globals] and [globals']. For each key
    [g] in at least one of [globals] and [globals'], [f g x y] determines the
    value in the resultant global environment, where [x] and [y] are the
    bindings corresponding to [g] in [globals] and [globals'] respectively. *)
val merge: (Unique_global.t -> 'a option -> 'b option -> 'c option)
  -> 'a t -> 'b t -> 'c t

(** [union f globals globals'] computes a global environment whose keys are a
    subset of the union of the keys of [globals] and [globals']. When the same
    binding is defined in both arguments, the function f is used to combine
    them. This is a special case of {!val:merge}: [union f globals globals'] is
    equivalent to [merge f' globals globals'], where

    - [f' _key None None = None]
    - [f' _key (Some v) None = Some v]
    - [f' _key None (Some v) = Some v]
    - [f' key (Some v1) (Some v2) = f key v1 v2]  
  *)
val union: (Unique_global.t -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

(** [remove qname globals] removes the last binding to be associated with
    [qname]. If no such binding exists, [globals] is returned unchanged. *)
val remove: Unique_global.t -> 'a t -> 'a t

(** [find qname globals] is the value bound to [qname] in [globals] or @raise
    Not_found if no such binding exists. *)
val find: Unique_global.t -> 'a t -> 'a

(** [find_opt] is like {!val:find}, but it returns [None] rather than raising
    an exception. *)
val find_opt: Unique_global.t -> 'a t -> 'a option

(** [mem qname globals] is [true] if there is a value bound to [name] in
    [globals] and [false] otherwise. *)
val mem: Unique_global.t -> 'a t -> bool

(** [switch path globals] is a globals structure with all the bindings present
    at [path] or @raise Not_found if there are no bindings at [path]. *)
val switch: Ident.t list -> 'a t -> 'a t

(** [switch_opt] is like {!val:switch} but it returns [None] rather than raising
    an exception. *)
val switch_opt: Ident.t list -> 'a t -> 'a t option

(** [iter f globals] applies [f] to every element of [globals] in unspecified
    order. *)
val iter: (Unique_global.t -> 'a -> unit) -> 'a t -> unit

(** [fold f init globals] folds over the bindings in [globals] using the
    function [f] and [init] as the initial value for the accumulator in
    unspecified order. *)
val fold: (Unique_global.t -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc