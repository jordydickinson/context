module Index = Local.Index
module Level = Local.Level
module Pool = Local.Pool

type index = Index.t
type level = Level.t

type _ t =
| Indexed : Ident.t * index -> index t
| Leveled : Ident.t * level -> level t

type indexed = index t
type leveled = level t

type space = Local.pool Ident.Map.t

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

module Space = struct
  type t = space

  let empty = Ident.Map.empty

  let is_empty = Ident.Map.is_empty

  let subspace sp1 sp2 = sp1 |> Ident.Map.for_all begin fun id pool1 ->
    match Ident.Map.find_opt id sp2 with
    | None -> false
    | Some pool2 -> Pool.subpool pool1 pool2
  end

  let union = Ident.Map.union (fun _ pool1 pool2 -> Some (Pool.union pool1 pool2))

  let pool id space =
    Ident.Map.find_opt id space
    |> Option.value ~default:Pool.empty

  let extend id = Ident.Map.update id begin function
  | None -> Some (Pool.extend Pool.empty)
  | Some pool -> Some (Pool.extend pool)
  end

  let shrink id = Ident.Map.update id begin function
  | None -> raise Not_found
  | Some pool ->
    let pool = Pool.shrink pool in
    if Pool.is_empty pool then None else Some pool
  end

  let shrink_opt id space = try Some (shrink id space) with Not_found -> None

  let mem name space = match Ident.Map.find_opt (ident name) space with
  | None -> false
  | Some pool -> Pool.mem (to_local name) pool
end