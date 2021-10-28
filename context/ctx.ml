type (+'local, +'global, +'scope) t =
  { vars: ('local, 'global) Vars.t
  ; scope: 'scope Scope.t
  }

let make globals =
  let vars = Vars.make Locals.empty globals in
  let scope = Scope.root in
  { vars; scope }

let vars ctx = ctx.vars

let scope ctx = ctx.scope

let locals ctx = ctx.vars.locals

let globals ctx = ctx.vars.globals

let add ?data v ctx =
  let vars = Vars.update_locals (Locals.add v) ctx.vars in
  let scope = Scope.enter ?data ctx.scope in
  { vars; scope }

let pop_opt ctx = match Locals.pop_opt ctx.vars.locals with
| None -> None
| Some (x, locals) ->
  let vars = Vars.set_locals locals ctx.vars in
  let scope = Scope.exit ctx.scope in
  Some (x, { vars; scope })

let pop ctx = match pop_opt ctx with
| None -> raise Not_found
| Some xctx -> xctx

let drop_opt ctx = Option.map snd (pop_opt ctx)

let drop ctx = match drop_opt ctx with
| None -> raise Not_found
| Some ctx -> ctx

