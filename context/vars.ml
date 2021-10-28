type (+'local, +'global) t =
  { locals: 'local Locals.t
  ; globals: 'global Globals.t
  }

let empty = { locals = Locals.empty; globals = Globals.empty }

let is_empty vars = Locals.is_empty vars.locals && Globals.is_empty vars.globals

let make locals globals = { locals; globals }

let locals vars = vars.locals

let globals vars = vars.globals

let set_locals locals vars = { vars with locals }

let set_globals globals vars = { vars with globals }

let update_locals f vars = { vars with locals = f vars.locals }

let update_globals f vars = { vars with globals = f vars.globals }