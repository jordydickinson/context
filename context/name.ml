module Index = Local.Index
module Level = Local.Level

type index = Index.t
type level = Level.t

type _ t =
| Indexed : Ident.t * index -> index t
| Leveled : Ident.t * level -> level t

type indexed = index t
type leveled = level t

let indexed id i = Indexed (id, i)

let leveled id l = Leveled (id, l)

let ident (type a) : a t -> _ = function
| Indexed (id, _) -> id
| Leveled (id, _) -> id

let to_local (type a) : a t -> a Local.t = function
| Indexed (_, i) -> Index i
| Leveled (_, l) -> Level l

let drop_ident (type a) : a t -> a = function
| Indexed (_, i) -> i
| Leveled (_, l) -> l

let equal (type a) (x: a t) (y: a t) : bool = match x, y with
| Indexed (id, i), Indexed (id', i') -> Ident.equal id id' && Index.equal i i'
| Indexed _, _ -> false
| Leveled (id, l), Leveled (id', l') -> Ident.equal id id' && Level.equal l l'
| Leveled _, _ -> false