type index = Local.index
type level = Local.level

type (_, _) t =
| Local : 'l Local.t -> ('l, _) t
| Global : 'g Global.t -> (_, 'g) t

type indexed = (index, index) t

type leveled = (level, level) t