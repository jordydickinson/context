type t = string

let of_string = function
| "" -> failwith "empty identifier"
| x -> x

let to_string x = x

let pp = Format.pp_print_string

let to_sexp x : Sexp.t = Atom (Symbol x)

let equal = String.equal

include Comparable.Make (struct
  type nonrec t = t
  let compare = String.compare
end)