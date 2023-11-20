type index = Local.index
type level = Local.level

type (_, _) t =
| Local : 'l Local.t -> ('l, _) t
| Global : 'g Global.t -> (_, 'g) t

type indexed = (index, index) t

type indexed_leveled = (index, level) t

type leveled = (level, level) t

type leveled_indexed = (level, index) t

let equal (type l g) (x: (l, g) t) (y: (l, g) t) : bool = match x, y with
| Local l, Local l' -> Local.equal l l'
| Local _, _ -> false
| Global g, Global g' -> Global.equal g g'
| Global _, _ -> false

let compare (type l g) (x: (l, g) t) (y: (l, g) t) : int = match x, y with
| Local l, Local l' -> Local.compare l l'
| Local _, Global _ -> -1
| Global _, Local _ -> 1
| Global g, Global g' -> Global.compare g g'

module Indexed = struct
  module T = struct
    type t = indexed

    let equal = equal
    let compare = compare
  end
  include T

  include Comparable.Make (T)
end

module Indexed_leveled = struct
  module T = struct
    type t = indexed_leveled

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

module Leveled_indexed = struct
  module T = struct
    type t = leveled_indexed

    let equal = equal
    let compare = compare
  end
  include T

  include Comparable.Make (T)
end