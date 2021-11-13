(** Identifiers, essentially glorified strings. The type distinction is to
    reduce type errors. *)
type t = private string

(** Create an identifier from a string.

    @raise [Failure] if the string is empty. *)
val of_string: string -> t

include Stringable.S with type t := t
include Pretty.S with type t := t
include Equatable.S with type t := t
include Comparable.S with type t := t