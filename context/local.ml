type index = int
type level = int
type pool = int

type _ t =
| Index : index -> index t
| Level : level -> level t

let equal (type a) (db: a t) (db': a t) = match db, db' with
| Level l, Level l' -> Int.equal l l'
| Level _, _ -> false
| Index i, Index i' -> Int.equal i i'
| Index _, _ -> false

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

  let equal p1 p2 = Int.equal p1 p2

  let subpool p1 p2 = p1 < p2

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
  | Level l -> mem_level l pool
  | Index i -> mem_index i pool

  let level_of_index_opt pool i =
    if not @@ mem_index i pool then None else Option.some @@
    Level.of_index ~size:pool i

  let level_of_index pool i = match level_of_index_opt pool i with
  | None -> invalid_arg "index not in pool"
  | Some l -> l

  let index_of_level_opt pool l =
    if not @@ mem_level l pool then None else Option.some @@
    Index.of_level ~size:pool l

  let index_of_level pool l = match index_of_level_opt pool l with
  | None -> invalid_arg "level not in pool"
  | Some i -> i

  let to_level_opt (type a) pool : a db -> _ = function
  | Level l -> Some l
  | Index i -> level_of_index_opt pool i

  let to_level pool db = match to_level_opt pool db with
  | None -> invalid_arg "not in pool"
  | Some l -> l

  let to_index_opt (type a) pool : a db -> _ = function
  | Level l -> index_of_level_opt pool l
  | Index i -> Some i

  let to_index pool db = match to_index_opt pool db with
  | None -> invalid_arg "not in pool"
  | Some l -> l
end