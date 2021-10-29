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