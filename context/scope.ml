module Level = Local.Level
module Pool = Local.Pool

type pool = Local.pool

type +'a t = { data: 'a Level.Map.t; pool: pool }

let root = { data = Level.Map.empty; pool = Pool.empty }

let is_root scope = Pool.is_empty scope.pool

let enter ?data scope = match data with
| None -> { scope with pool = Pool.extend scope.pool }
| Some v ->
  let l, pool = Pool.extend_level scope.pool in
  let data = Level.Map.add l v scope.data in
  { data; pool }

let exit_opt scope = match Pool.max_level_opt scope.pool with
| None -> None
| Some l ->
  let data = Level.Map.remove l scope.data in
  let pool = Pool.shrink scope.pool in
  Some { data; pool }

let exit scope = match exit_opt scope with
| None -> failwith "exit root"
| Some scope -> scope

let find_level_opt l scope = Level.Map.find_opt l scope.data

let find_index_opt i scope =
  let l = Pool.index_to_level scope.pool i in
  find_level_opt l scope

let find_opt (type a) : a Local.t -> _ = function
| Level l -> find_level_opt l
| Index i -> find_index_opt i

let find i scope = match find_opt i scope with
| None -> raise Not_found
| Some x -> x

let remove_level l scope = { scope with data = Level.Map.remove l scope.data }

let remove_index i scope =
  let l = Pool.index_to_level scope.pool i in
  remove_level l scope

let remove (type a) : a Local.t -> _ = function
| Level l -> remove_level l
| Index i -> remove_index i

let update_level l f scope =
  if not @@ Pool.mem_level l scope.pool then scope else
  { scope with data = Level.Map.update l f scope.data }

let update_index i f scope =
  let l = Pool.index_to_level scope.pool i in
  update_level l f scope

let update (type a) : a Local.t -> _ = function
| Level l -> update_level l
| Index i -> update_index i