module Index = Local.Index
module Level = Local.Level
module Pool = Local.Pool

type +'a t = 'a Locals.t Ident.Map.t

type 'a change =
| Add of 'a
| Remove
| Replace of 'a

let empty = Ident.Map.empty

let is_empty = Ident.Map.is_empty

let add id v = Ident.Map.update id begin function
| None -> Some (Locals.singleton v)
| Some locals -> Some (Locals.add v locals)
end

let singleton id v = Ident.Map.singleton id (Locals.singleton v)

let remove id = Ident.Map.update id begin function
| None -> None
| Some locals ->
  let locals = Locals.drop locals in
  if Locals.is_empty locals then None else Some locals
end

let replace id v = Ident.Map.update id begin function
| None -> Some (Locals.singleton v)
| Some locals ->
  let locals = Locals.drop locals |> Locals.add v in
  Some locals
end

let change id f = Ident.Map.update id begin function
| None ->
  begin match f None with
  | Remove -> None
  | Add v | Replace v -> Some (Locals.singleton v)
  end
| Some locals ->
  let v = Locals.top locals in
  let locals = match f (Some v) with
  | Add v -> Some (Locals.add v locals)
  | Remove ->
    let locals = Locals.drop locals in
    if Locals.is_empty locals then None else Some locals
  | Replace v' when v == v' -> Some locals
  | Replace v -> Some (Locals.add v (Locals.drop locals))
  in
  locals
end

let update id f = change id begin fun x -> match f x with
| None -> Remove
| Some v -> Replace v
end

let import names = names |> Ident.Map.union begin fun _ locals1 locals2 ->
  Some (List.fold_left (Fun.flip Locals.add) locals2 locals1.elts)
end

let to_ident_map names =
  names |> Ident.Map.map begin fun locals ->
    assert (not @@ Locals.is_empty locals);
    if Locals.length locals <> 1 then invalid_arg "Shadowed binding";
    Locals.top locals
  end

let map f = Ident.Map.map (Locals.map f)

let iter f = Ident.Map.iter (fun id locals -> Locals.iteri (f id) locals)

let fold_left f init names =
  let folder id locals acc =
    let folder (i, acc) x =
      let acc = f id i acc x in
      let i = Index.succ i in
      i, acc
    in 
    let init = Index.of_int 0, acc in
    List.fold_left folder init Locals.(locals.elts)
    |> snd
  in
  Ident.Map.fold folder names init

let fold_right f = Ident.Map.fold begin fun id locals acc ->
  let folder x (l, acc) =
    let acc = f id l x acc in
    let l = Level.succ l in
    l, acc
  in
  let init = Level.zero, acc in
  List.fold_right folder Locals.(locals.elts) init
  |> snd
end

let combine names1 names2 =
  List.map2
    begin fun (id1, locals1) (id2, locals2) ->
      if not @@ Ident.equal id1 id2 then invalid_arg "Differing bindings";
      id1, Locals.combine locals1 locals2
    end
    (Ident.Map.bindings names1)
    (Ident.Map.bindings names2)
  |> Ident.Map.of_list

let iter2 f names1 names2 =
  List.iter2
    begin fun (id1, locals1) (id2, locals2) ->
      if not @@ Ident.equal id1 id2 then invalid_arg "Differing bindings";
      Locals.iteri2 (f id1) locals1 locals2
    end
    (Ident.Map.bindings names1)
    (Ident.Map.bindings names2)

let find_opt name names = match Ident.Map.find_opt (Name.ident name) names with
| None -> None
| Some locals -> Locals.get_opt (Name.to_local name) locals

let find name names = match find_opt name names with
| None -> raise Not_found
| Some v -> v

let mem name names = match Ident.Map.find_opt (Name.ident name) names with
| None -> false
| Some locals -> Locals.mem (Name.to_local name) locals

let pool name names =
  Ident.Map.find_opt name names
  |> Option.value ~default:Locals.empty
  |> Locals.pool

let fix_opt name names =
  let id = Name.ident name in
  let pool = pool id names in
  let l = Pool.to_level_opt pool @@ Name.to_local name in
  Option.map (Name.leveled id) l

let fix name names = match fix_opt name names with
| None -> raise Not_found
| Some name -> name