(** Local or global variables. *)
type (_, _) t =
| Local : 'l Local.t -> ('l, _) t
| Global : 'g Global.t -> (_, 'g) t