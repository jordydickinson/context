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

let length xs = Pool.size xs.pool

let add x locals =
  let elts = x :: locals.elts
  and pool = Pool.fill locals.pool in
  { elts; pool }

let of_list elts =
  let pool = Pool.fill ~amt:(List.length elts) Pool.empty in
  { elts; pool }

let append locals locals' =
  let elts = locals.elts @ locals'.elts in
  let pool = Pool.fill ~amt:(Pool.size locals'.pool) locals.pool in
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
  let locals = { elts; pool = Pool.drain locals.pool } in
  Some (x, locals)

let pop locals = match pop_opt locals with
| None -> failwith "pop empty"
| Some xxs -> xxs

let drop_opt locals = match locals.elts with
| [] -> None
| _ :: elts -> Some { elts; pool = Pool.drain locals.pool }

let drop locals = match drop_opt locals with
| None -> failwith "drop empty"
| Some ocals -> ocals

let ith i locals = List.nth locals.elts (Index.to_int i)

let ith_opt i locals = List.nth_opt locals.elts (Index.to_int i)

let lth_opt l locals = match Pool.index_of_level_opt locals.pool l with
| None -> None
| Some i -> ith_opt i locals

let lth l locals = match lth_opt l locals with
| None -> raise Not_found
| Some v -> v

let get_opt (type a) : a Local.t -> _ = function
| Indexed i -> ith_opt i
| Leveled l -> lth_opt l

let get i locals = match get_opt i locals with
| None -> raise Not_found
| Some v -> v

let mem i locals = Pool.mem i locals.pool

let fix_opt (type l) (local: l Local.t) (locals: 'a t) =
  Local.Pool.to_level_opt locals.pool local
  |> Option.map (fun l -> Local.Leveled l)

let fix local locals =
  match fix_opt local locals with
  | None -> raise Not_found
  | Some local -> local

let iter f xs = List.iter f xs.elts

let iteri f xs = List.iteri (fun i -> f (Local.Index.of_int i)) xs.elts

let map f xs = { xs with elts = List.map f xs.elts }

let combine xs ys =
  if not @@ Pool.equal xs.pool ys.pool then invalid_arg "Differing bindings";
  { xs with elts = List.combine xs.elts ys.elts }

let iter2 f xs ys = List.iter2 f xs.elts ys.elts

let iteri2 f xs ys =
  let rec iteri2' i xs ys = match xs, ys with
  | [], [] -> ()
  | [], _ | _, [] -> invalid_arg "Differing bindings"
  | x :: xs, y :: ys ->
    f (Local.Index.of_int i) x y;
    iteri2' (i + 1) xs ys
  in
  iteri2' 0 xs.elts ys.elts

let map2 f xs ys =
  if not @@ Pool.equal xs.pool ys.pool then invalid_arg "Differing bindings";
  { xs with elts = List.map2 f xs.elts ys.elts }

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
    let l = Pool.level_of_index locals.pool i in
    pred l x
  in
  match findi_opt pred' locals with
  | None -> None
  | Some (i, x) -> Some (Pool.level_of_index locals.pool i, x)

let findl pred locals = match findl_opt pred locals with
| None -> raise Not_found
| Some x -> x

let index pred locals = fst (findi (fun _ -> pred) locals)

let index_opt pred locals = Option.map fst (findi_opt (fun _ -> pred) locals)

let level pred locals = fst (findl (fun _ -> pred) locals)

let level_opt pred locals = Option.map fst (findl_opt (fun _ -> pred) locals)