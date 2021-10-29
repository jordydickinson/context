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

let fold_left f init names =
  let folder id locals acc =
    let folder (i, acc) x =
      let acc = f id i acc x in
      let i = Debruijn.Index.succ i in
      i, acc
    in 
    let init = Debruijn.Index.of_int 0, acc in
    List.fold_left folder init Locals.(locals.elts)
    |> snd
  in
  Ident.Map.fold folder names init

let fold_right f = Ident.Map.fold begin fun id locals acc ->
  let folder x (l, acc) =
    let acc = f id l x acc in
    let l = Debruijn.Level.succ l in
    l, acc
  in
  let init = Debruijn.Level.zero, acc in
  List.fold_right folder Locals.(locals.elts) init
  |> snd
end

let find_opt name names = match Ident.Map.find_opt (Name.ident name) names with
| None -> None
| Some locals -> Locals.get_opt (Name.debruijn name) locals

let find name names = match find_opt name names with
| None -> raise Not_found
| Some v -> v

let mem name names = match Ident.Map.find_opt (Name.ident name) names with
| None -> false
| Some locals -> Locals.mem (Name.debruijn name) locals