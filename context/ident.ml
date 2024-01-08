type t = string

let of_string = function
| "" -> failwith "empty identifier"
| x -> x

let to_string x = x

let pp = Format.pp_print_string

let equal = String.equal

include Comparable.Make (struct
  type nonrec t = t
  let compare = String.compare
end)

include Hashable.Make (struct
  type nonrec t = t
  let equal = equal
  let hash = String.hash
end)