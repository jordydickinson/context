module Index = Local.Index
module Pool = Local.Pool
type pool = Local.pool

type +'a t =
  { elts: 'a list
  ; pool: pool
  }

let empty =
  { elts = []
  ; pool = Pool.empty
  }

let is_empty = function
| { elts = []; pool } -> assert (Pool.is_empty pool); true
| _ -> false

let pool locals = locals.pool

let elts locals = locals.elts

let add x locals =
  let elts = x :: locals.elts
  and pool = Pool.extend locals.pool in
  { elts; pool }

let singleton x = add x empty

let top_opt locals = match locals.elts with
| [] -> None
| x :: _ -> Some x

let top locals = match top_opt locals with
| None -> failwith "top empty"
| Some x -> x

let pop_opt locals = match locals.elts with
| [] -> None
| x :: elts ->
  let locals = { elts; pool = Pool.shrink locals.pool } in
  Some (x, locals)

let pop locals = match pop_opt locals with
| None -> failwith "pop empty"
| Some xxs -> xxs

let drop_opt locals = match locals.elts with
| [] -> None
| _ :: elts -> Some { elts; pool = Pool.shrink locals.pool }

let drop locals = match drop_opt locals with
| None -> failwith "drop empty"
| Some ocals -> ocals

let ith i locals = List.nth locals.elts (Index.to_int i)

let ith_opt i locals = List.nth_opt locals.elts (Index.to_int i)

let lth i locals =
  let i = Pool.level_to_index locals.pool i in
  ith i locals

let lth_opt i locals =
  let i = Pool.level_to_index locals.pool i in
  ith_opt i locals

let get_opt (type a) : a Local.t -> _ = function
| Bound i -> ith_opt i
| Free l -> lth_opt l

let get i locals = match get_opt i locals with
| None -> raise Not_found
| Some v -> v

let mem i locals = Pool.mem i locals.pool

let map f xs = { xs with elts = List.map f xs.elts }

let find pred locals = List.find pred locals.elts

let find_opt pred locals = List.find_opt pred locals.elts

let findi_opt pred locals =
  let rec findi' i = function
  | [] -> None
  | x :: xs ->
    let i' = Index.of_int i in
    if pred i' x
    then Some (i', x)
    else findi' (i + 1) xs
  in
  findi' 0 locals.elts

let findi pred locals = match findi_opt pred locals with
| None -> raise Not_found
| Some x -> x

let findl_opt pred locals =
  let pred' i x =
    let l = Pool.index_to_level locals.pool i in
    pred l x
  in
  match findi_opt pred' locals with
  | None -> None
  | Some (i, x) -> Some (Pool.index_to_level locals.pool i, x)

let findl pred locals = match findl_opt pred locals with
| None -> raise Not_found
| Some x -> x

let index pred locals = fst (findi (fun _ -> pred) locals)

let index_opt pred locals = Option.map fst (findi_opt (fun _ -> pred) locals)

let level pred locals = fst (findl (fun _ -> pred) locals)

let level_opt pred locals = Option.map fst (findl_opt (fun _ -> pred) locals)