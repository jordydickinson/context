module Index = Local.Index

type +'a entry =
| Leaf of 'a
| Branch of 'a option * 'a t

and +'a t = 'a entry Names.t

let empty = Names.empty

let is_empty = Names.is_empty

let rec singleton path id v = match path with
| [] -> Names.singleton id (Leaf v)
| step :: path -> Names.singleton step (Branch (None, singleton path id v))

let rec qualify path ns = match path with
| [] -> ns
| step :: path -> Names.singleton step (Branch (None, qualify path ns))

let rec add path id v = match path with
| [] -> Names.add id (Leaf v)
| step :: path ->
  Names.update step begin function
  | None -> Some (Branch (None, singleton path id v))
  | Some Leaf v' -> Some (Branch (Some v', singleton path id v))
  | Some Branch (v', globals) -> Some (Branch (v', add path id v globals))
  end

let rec import path ns = match path with
| [] ->
  ns |> Names.fold_right begin fun id _ -> function
  | Leaf v -> add [] id v
  | Branch (None, ns') -> import [id] ns'
  | Branch (Some v, ns') -> fun ns -> import [] ns' ns |> add [] id v
  end
| step :: path ->
  Names.update step begin function
  | None -> Some (Branch (None, qualify path ns))
  | Some Leaf v -> Some (Branch (Some v, qualify path ns))
  | Some Branch (v, globals) -> Some (Branch (v, import path ns globals))
  end

let rec remove path id = match path with
| [] ->
  Names.update id begin function
  | None -> None
  | Some Leaf _ -> None
  | Some Branch (_, globals) -> Some (Branch (None, globals))
  end
| step :: path ->
  Names.update step begin function
  | None | Some Leaf _ as entry -> entry
  | Some Branch (v, globals) ->
    let globals = remove path id globals in
    if not @@ is_empty globals
    then Some (Branch (v, globals))
    else Option.map (fun v -> Leaf v) v
  end

let rec find_opt Global.{ path; name } globals = match path with
| [] ->
  begin match Names.find_opt name globals with
  | None | Some Branch (None, _) -> None
  | Some Leaf v | Some Branch (Some v, _) -> Some v
  end
| step :: path ->
  begin match Names.find_opt step globals with
  | None | Some Leaf _ -> None
  | Some Branch (_, globals) -> find_opt { path; name } globals
  end

let find qname globals = match find_opt qname globals with
| None -> raise Not_found
| Some v -> v

let mem qname globals = Option.is_some (find_opt qname globals)

let rec switch_opt path globals = match path with
| [] -> Some globals
| step :: path ->
  begin match Names.find_opt step globals with
  | None | Some Leaf _ -> None
  | Some Branch (_, globals) -> switch_opt path globals
  end

let switch path globals = match switch_opt path globals with
| None -> raise Not_found
| Some globals -> globals

let pool path id globals =
  switch_opt path globals
  |> Option.value ~default:Names.empty
  |> Names.pool id

let fix_path_opt path globals =
  let rec fix_path_opt' globals = let open Option.O in function
  | [] -> Some []
  | name :: path ->
    let* name = Names.fix_opt name globals in
    let+ path = fix_path_opt' (switch [name] globals) path in
    name :: path
  in
  fix_path_opt' globals path

let fix_opt Global.{ path; name } globals =
  let open Option.O in
  let* path = fix_path_opt path globals in
  let+ name = Names.fix_opt name (switch path globals) in
  Global.make path name

let fix global globals = match fix_opt global globals with
| None -> raise Not_found
| Some global -> global

let iter f =
  let rec iter' path =
    Names.iter begin fun id index -> function
    | Leaf x -> f path id index x
    | Branch (x, globals) ->
      Option.iter (f path id index) x;
      iter' (path @ [id]) globals
    end
  in
  iter' []