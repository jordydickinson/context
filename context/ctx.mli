(** A type providing read-only access to globals and read-write access to locals
    and scope data while keeping locals and scope data in-sync. *)
type (+'local, +'global, +'scope) t = private
  { vars: ('local, 'global) Vars.t
  ; scope: 'scope Scope.t
  }

(** [make globals] is a new context in the root scope with all the bindings of
    [globals]. *)
val make: 'g Globals.t -> ('l, 'g, 's) t

(** [vars ctx] are the local and global bindings of [ctx]. *)
val vars: ('l, 'g, 's) t -> ('l, 'g) Vars.t

(** [scope ctx] is the current open scope of [ctx]. *)
val scope: ('l, 'g, 's) t -> 's Scope.t

(** [locals ctx] are all the local bindings of [ctx]. *)
val locals: ('l, 'g, 's) t -> 'l Locals.t

(** [globals ctx] are all the global bindings of [ctx]. *)
val globals: ('l, 'g, 's) t -> 'g Globals.t

(** [add ?data v ctx] is [ctx] with a new open scope whose variable is bound
    to [v] and optionally associated with [data], if provided. *)
val add: ?data:'s -> 'l -> ('l, 'g, 's) t -> ('l, 'g, 's) t

(** [pop ctx] is a pair [v, ctx'] where [v] is the value bound to the local
    variable of the most-recently-opened scope and [ctx'] is [ctx] with the same
    scope closed.
    
    @raise Failure if [ctx] is in the root scope
  *)
val pop: ('l, 'g, 's) t -> 'l * ('l, 'g, 's) t

(** [pop_opt] is like {!val:pop}, but it returns [None] rather than raising an
    exception. *)
val pop_opt: ('l, 'g, 's) t -> ('l * ('l, 'g, 's) t) option

(** [drop ctx] is equivalent to [snd @@ pop ctx].

    @raise Failure if [ctx] is in the root scope
  *)
val drop: ('l, 'g, 's) t -> ('l, 'g, 's) t

(** [drop_opt] is like {!val:drop} but it returns [None] rather than raising an
    exception. *)
val drop_opt: ('l, 'g, 's) t -> ('l, 'g, 's) t option

(** [map_locals f ctx] is a context with all the bindings and scopes of [ctx],
    but all local bindings have been replaced by the application of [f] to the
    binding. *)
val map_locals: ('a -> 'b) -> ('a, 'g, 's) t -> ('b, 'g, 's) t