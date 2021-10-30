type index := Local.index
type level := Local.level

(** Local or global variables. *)
type (_, _) t =
| Local : 'l Local.t -> ('l, _) t
| Global : 'g Global.t -> (_, 'g) t

(** Indexed variables *)
type indexed = (index, index) t

(** Leveled variables *)
type leveled = (level, level) t