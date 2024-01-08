type index = Index of int
type level = Level of int
type pool = Pool of int

type _ t =
| Indexed : index -> index t
| Leveled : level -> level t

type indexed = index t
type leveled = level t

let equal (type a) (db: a t) (db': a t) = match db, db' with
| Leveled Level l, Leveled Level l' -> Int.equal l l'
| Indexed Index i, Indexed Index i' -> Int.equal i i'

let compare (type a) (db: a t) (db': a t) = match db, db' with
| Leveled Level l, Leveled Level l' -> Int.compare l l'
| Indexed Index i, Indexed Index i' -> Int.compare i i'

let hash (type a) (db: a t) = match db with
| Leveled Level l -> Int.hash l
| Indexed Index i -> Int.hash i

module Index =
struct
  module T = struct
    type t = index

    let equal (Index i1) (Index i2) = Int.equal i1 i2

    let compare (Index i1) (Index i2) = Int.compare i1 i2

    let hash (Index i) = Int.hash i
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let of_int i = Index i

  let to_int (Index i) = i

  let succ (Index i) = Index (i + 1)

  let pred (Index i) =
    if i = 0 then invalid_arg "no predecessor";
    Index (i - 1)

  let of_level ~size (Level i) = Index (size - (i + 1))

  let shift ?(min = Index 0) ~amt (Index i) =
    let Index min = min in
    if i >= min then Index (i + amt) else Index i
end

module Level =
struct
  module T = struct
    type t = level

    let equal (Level i1) (Level i2) = Int.equal i1 i2

    let compare (Level i1) (Level i2) = Int.compare i1 i2

    let hash (Level i) = Int.hash i
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let zero = Level 0

  let succ (Level i) = Level (i + 1)

  let of_index ~size (Index i) = Level (size - (i + 1))
end

module Pool =
struct
  type 'a db = 'a t
  type t = pool

  let empty = Pool 0

  let is_empty (Pool pool) = pool = 0

  let equal (Pool p1) (Pool p2) = Int.equal p1 p2

  let subpool (Pool p1) (Pool p2) = p1 < p2

  let union (Pool p1) (Pool p2) = Pool (max p1 p2)

  let size (Pool pool) = pool

  let of_level (Level l) = Pool (l + 1)

  let max_level_opt (Pool pool) =
    assert (pool >= 0);
    if pool = 0 then None else Some (Level (pool - 1))

  let max_level pool = match max_level_opt pool with
  | None -> failwith "max_level empty"
  | Some l -> l

  let next_level (Pool pool) = Level pool

  let fill ?(amt = 1) (Pool pool) =
    if amt < 0 then invalid_arg "negative amt";
    Pool (pool + amt)

  let drain ?(amt = 1) (Pool pool) =
    if amt < 0 then invalid_arg "negative amt";
    if pool < amt then failwith "pool size too low";
    Pool (pool - amt)

  let mem_index (Index i) (Pool pool) = i < pool

  let mem_level (Level i) (Pool pool) = i < pool

  let mem (type a) (db: a db) pool = match db with
  | Leveled l -> mem_level l pool
  | Indexed i -> mem_index i pool

  let level_of_index_opt (Pool size as pool) i =
    if not @@ mem_index i pool then None else Option.some @@
    Level.of_index ~size i

  let level_of_index pool i = match level_of_index_opt pool i with
  | None -> invalid_arg "index not in pool"
  | Some l -> l

  let index_of_level_opt (Pool size as pool) l =
    if not @@ mem_level l pool then None else Option.some @@
    Index.of_level ~size l

  let index_of_level pool l = match index_of_level_opt pool l with
  | None -> invalid_arg "level not in pool"
  | Some i -> i

  let to_level_opt (type a) pool : a db -> _ = function
  | Leveled l -> Some l
  | Indexed i -> level_of_index_opt pool i

  let to_level pool db = match to_level_opt pool db with
  | None -> invalid_arg "not in pool"
  | Some l -> l

  let to_index_opt (type a) pool : a db -> _ = function
  | Leveled l -> index_of_level_opt pool l
  | Indexed i -> Some i

  let to_index pool db = match to_index_opt pool db with
  | None -> invalid_arg "not in pool"
  | Some l -> l
end