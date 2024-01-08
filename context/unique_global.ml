module T = struct
  type t =
    { path: Ident.t list
    ; ident: Ident.t
    }

  let equal q q' =
    List.equal Ident.equal q.path q'.path &&
    Ident.equal q.ident q'.ident

  let compare q q' =
    let cmp_path = List.compare Ident.compare q.path q'.path in
    if cmp_path <> 0 then cmp_path else
    Ident.compare q.ident q'.ident

  let hash q =
    let hpath = List.hash Ident.hash q.path in
    let hident = Ident.hash q.ident in
    hash_combine hpath hident
end
include T
include Comparable.Make(T)
include Hashable.Make(T)

let make path ident = { path; ident }