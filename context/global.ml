type index = Local.index
type level = Local.level

type 'a t =
  { path: 'a Name.t list
  ; name: 'a Name.t
  }

type indexed = index t

type leveled = level t

let make path name = { path; name }

let equal q q' =
  List.equal Name.equal q.path q'.path &&
  Name.equal q.name q'.name

let compare q q' =
  let cmp_path = List.compare Name.compare q.path q'.path in
  if cmp_path <> 0 then cmp_path else
  Name.compare q.name q'.name

module Indexed = struct
  module T = struct
    type t = indexed

    let equal = equal
    let compare = compare
  end
  include T

  include Comparable.Make (T)
end

module Leveled = struct
  module T = struct
    type t = leveled

    let equal = equal
    let compare = compare
  end
  include T

  include Comparable.Make (T)
end