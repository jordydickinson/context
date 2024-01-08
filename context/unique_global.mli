type t =
  { path: Ident.t list
  ; ident: Ident.t
  }

include Equatable.S with type t := t
include Comparable.S with type t := t
include Hashable.S with type t := t

val make: Ident.t list -> Ident.t -> t