type +'a entry =
| Leaf of 'a
| Branch of 'a option * 'a t

and +'a t = 'a entry Ident.Map.t

let empty = Ident.Map.empty

let is_empty = Ident.Map.is_empty

let rec singleton Unique_global.{ path; ident } v = match path with
| [] -> Ident.Map.singleton ident (Leaf v)
| step :: path -> Ident.Map.singleton step (Branch (None, singleton { path; ident } v))

let rec qualify path globals = match path with
| [] -> globals
| step :: path -> Ident.Map.singleton step (Branch (None, qualify path globals))

let rec add Unique_global.{ path; ident } v = match path with
| [] -> Ident.Map.add ident (Leaf v)
| step :: path ->
  Ident.Map.update step begin function
  | None -> Some (Branch (None, singleton { path; ident } v))
  | Some Leaf v' -> Some (Branch (Some v', singleton { path; ident } v))
  | Some Branch (v', globals) -> Some (Branch (v', add { path; ident } v globals))
  end

let rec includ path ns = match path with
| [] ->
  ns |> Ident.Map.fold begin fun ident -> function
  | Leaf v -> add { path = []; ident } v
  | Branch (None, ns') -> includ [ident] ns'
  | Branch (Some v, ns') -> fun ns -> includ [] ns' ns |> add { path = []; ident } v
  end
| step :: path ->
  Ident.Map.update step begin function
  | None -> Some (Branch (None, qualify path ns))
  | Some Leaf v -> Some (Branch (Some v, qualify path ns))
  | Some Branch (v, globals) -> Some (Branch (v, includ path ns globals))
  end

let rec import Unique_global.{ path; ident } ns = match path with
| [] -> Ident.Map.add ident (Branch (None, ns))
| step :: path ->
  Ident.Map.update step begin function
  | None -> Some (Branch (None, qualify path ns))
  | Some Leaf v -> Some (Branch (Some v, qualify path ns))
  | Some Branch (v, globals) -> Some (Branch (v, import { path; ident } ns globals))
  end

let merge f =
  let rec merge' path =
    Ident.Map.merge begin fun id entry entry' ->
      let g = Unique_global.make path id in
      match entry, entry' with
    | None, None -> Option.map (fun v -> Leaf v) (f g None None)
    | None, Some Leaf v -> Option.map (fun v -> Leaf v) (f g None (Some v))
    | None, Some Branch (None, globals) ->
      let globals = merge' (path @ [id]) empty globals in
      if is_empty globals then None else Some (Branch (None, globals))
    | None, Some Branch (Some v, globals) ->
      let globals = merge' (path @ [id]) empty globals in
      let v = f g None (Some v) in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Leaf v, None -> Option.map (fun v -> Leaf v) (f g (Some v) None)
    | Some Leaf v, Some Leaf v' -> Option.map (fun v -> Leaf v) (f g (Some v) (Some v'))
    | Some Leaf v, Some Branch (None, globals) ->
      let globals = merge' (path @ [id]) empty globals in
      let v = f g (Some v) None in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Leaf v, Some Branch (Some v', globals) ->
      let globals = merge' (path @ [id]) empty globals in
      let v = f g (Some v) (Some v') in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Branch (None, globals), None ->
      let globals = merge' (path @ [id]) globals empty in
      if is_empty globals
      then None
      else Some (Branch (None, globals))
    | Some Branch (None, globals), Some Leaf v ->
      let globals = merge' (path @ [id]) globals empty in
      let v = f g None (Some v) in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Branch (None, globals), Some Branch (None, globals') ->
      let globals = merge' (path @ [id]) globals globals' in
      if is_empty globals then None else Some (Branch (None, globals))
    | Some Branch (None, globals), Some Branch (Some v, globals') ->
      let globals = merge' (path @ [id]) globals globals' in
      let v = f g None (Some v) in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Branch (Some v, globals), None ->
      let globals = merge' (path @ [id]) globals empty in
      let v = f g (Some v) None in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Branch (Some v, globals), Some Leaf v' ->
      let globals = merge' (path @ [id]) globals empty in
      let v = f g (Some v) (Some v') in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Branch (Some v, globals), Some Branch (None, globals') ->
      let globals = merge' (path @ [id]) globals globals' in
      let v = f g (Some v) None in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    | Some Branch (Some v, globals), Some Branch (Some v', globals') ->
      let globals = merge' (path @ [id]) globals globals' in
      let v = f g (Some v) (Some v') in
      if is_empty globals
      then Option.map (fun v -> Leaf v) v
      else Some (Branch (v, globals))
    end
  in
  merge' []

let union f = merge begin fun g v v' -> match v, v' with
| None, None -> None
| None, Some v | Some v, None -> Some v
| Some v, Some v' -> f g v v'
end

let rec remove Unique_global.{ path; ident } = match path with
| [] ->
  Ident.Map.update ident begin function
  | None -> None
  | Some Leaf _ -> None
  | Some Branch (_, globals) -> Some (Branch (None, globals))
  end
| step :: path ->
  Ident.Map.update step begin function
  | None | Some Leaf _ as entry -> entry
  | Some Branch (v, globals) ->
    let globals = remove { path; ident } globals in
    if not @@ is_empty globals
    then Some (Branch (v, globals))
    else Option.map (fun v -> Leaf v) v
  end

let rec find_opt Unique_global.{ path; ident } globals = match path with
| [] ->
  begin match Ident.Map.find_opt ident globals with
  | None | Some Branch (None, _) -> None
  | Some Leaf v | Some Branch (Some v, _) -> Some v
  end
| step :: path ->
  begin match Ident.Map.find_opt step globals with
  | None | Some Leaf _ -> None
  | Some Branch (_, globals) -> find_opt { path; ident } globals
  end

let find qname globals = match find_opt qname globals with
| None -> raise Not_found
| Some v -> v

let mem qname globals = Option.is_some (find_opt qname globals)

let rec switch_opt path globals = match path with
| [] -> Some globals
| step :: path ->
  begin match Ident.Map.find_opt step globals with
  | None | Some Leaf _ -> None
  | Some Branch (_, globals) -> switch_opt path globals
  end

let switch path globals = match switch_opt path globals with
| None -> raise Not_found
| Some globals -> globals

let iter (f: Unique_global.t -> _) =
  let rec iter' path =
    Ident.Map.iter begin fun ident -> function
    | Leaf x -> f { path; ident } x
    | Branch (x, globals) ->
      Option.iter (f { path; ident }) x;
      iter' (path @ [ident]) globals
    end
  in
  iter' []

let fold (f: Unique_global.t -> _) =
  let rec fold' path =
    Ident.Map.fold begin fun ident -> function
    | Leaf x -> f { path; ident } x
    | Branch (None, globals) -> fold' (path @ [ident]) globals
    | Branch (Some x, globals) -> fun acc ->
      let acc = f { path; ident } x acc in
      fold' (path @ [ident]) globals acc
    end
  in
  fold' []