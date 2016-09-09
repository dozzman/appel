open Types

module S = Symbol

type enventry =
| VarEntry of ty (* type of some variable *)
| FunEntry of ty list * ty (* type of some function *)

type tenv = ty S.table
type venv = enventry S.table

type env = {venv : venv; tenv : tenv}

let base_tenv () =
  let tenv1 = S.empty in
  let tenv2 = S.enter tenv1 (S.symbol "int") INT in
  let tenv3 = S.enter tenv2 (S.symbol "string") STRING in
  tenv3

let base_venv () : enventry S.table = S.empty

let empty () = {venv = base_venv (); tenv = base_tenv ()}
