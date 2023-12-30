type t =
  { path: Ident.t list
  ; ident: Ident.t
  }

include Equatable.S with type t := t

val make: Ident.t list -> Ident.t -> t

val compare: t -> t -> int