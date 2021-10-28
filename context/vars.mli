(** A simple structure combining {!type:Locals.t} and {!type:Globals.t}. *)
type (+'local, +'global) t =
  { locals: 'local Locals.t
  ; globals: 'global Globals.t
  }

(** [empty] contains no bindings. *)
val empty: ('l, 'g) t

(** [is_empty vars] is [true] if [vars] contains no bindings and [false]
    otherwise. *)
val is_empty: ('l, 'g) t -> bool

(** [make locals globals] contains all the bindings of [locals] and [globals]. *)
val make: 'l Locals.t -> 'g Globals.t -> ('l, 'g) t

(** [locals vars] are the local bindings of [vars]. *)
val locals: ('l, 'g) t -> 'l Locals.t

(** [globals vars] are the global bindings of [vars]. *)
val globals: ('l, 'g) t -> 'g Globals.t

(** [set_locals locals vars] is [vars] with all local bindings replaced by [locals]. *)
val set_locals: 'l Locals.t -> ('l, 'g) t -> ('l, 'g) t

(** [set_globals globals vars] is [vars] with all global bindings replaced by [globals]. *)
val set_globals: 'g Globals.t -> ('l, 'g) t -> ('l, 'g) t

(** [update_locals f vars] is equivalent to [set_locals (f @@ locals vars) vars]. *)
val update_locals: ('a Locals.t -> 'b Locals.t) -> ('a, 'g) t -> ('b, 'g) t

(** [update_globals f vars] is equivalent to [set_globals (f @@ globals vars) vars]. *)
val update_globals: ('a Globals.t -> 'b Globals.t) -> ('l, 'a) t -> ('l, 'b) t