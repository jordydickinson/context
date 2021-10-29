type index = int
type level = int
type pool = int

type _ t =
| Bound : index -> index t
| Free : level -> level t

let equal (type a) (db: a t) (db': a t) = match db, db' with
| Free l, Free l' -> Int.equal l l'
| Free _, _ -> false
| Bound i, Bound i' -> Int.equal i i'
| Bound _, _ -> false

module Index =
struct
  include Int

  let of_int i = i

  let to_int i = i

  let of_level ~size i = size - (i + 1)
end

module Level =
struct
  include Int

  let of_index ~size i = size - (i + 1)
end

module Pool =
struct
  type 'a db = 'a t
  type t = pool

  let empty = 0

  let is_empty pool = pool = 0

  let size pool = pool

  let max_level_opt pool =
    assert (pool >= 0);
    if pool = 0 then None else Some (pool - 1)

  let max_level pool = match max_level_opt pool with
  | None -> failwith "max_level empty"
  | Some l -> l

  let next_level pool = pool

  let extend pool = pool + 1

  let extend_level pool = pool, pool + 1

  let shrink pool =
    if pool > 0
    then pool - 1
    else failwith "cannot shrink empty pool"

  let mem_index i pool = i < pool

  let mem_level i pool = i < pool

  let mem (type a) (db: a db) pool = match db with
  | Free l -> mem_level l pool
  | Bound i -> mem_index i pool

  let index_to_level pool = Level.of_index ~size:pool

  let level_to_index pool = Index.of_level ~size:pool

  let to_level (type a) pool : a db -> _ = function
  | Free l -> l
  | Bound i -> index_to_level pool i

  let to_index (type a) pool : a db -> _ = function
  | Free l -> level_to_index pool l
  | Bound i -> i
end