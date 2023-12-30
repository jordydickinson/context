type t =
  { path: Ident.t list
  ; ident: Ident.t
  }

let make path ident = { path; ident }

let equal q q' =
  List.equal Ident.equal q.path q'.path &&
  Ident.equal q.ident q'.ident

let compare q q' =
  let cmp_path = List.compare Ident.compare q.path q'.path in
  if cmp_path <> 0 then cmp_path else
  Ident.compare q.ident q'.ident