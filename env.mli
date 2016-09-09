open Types

module S = Symbol

type enventry =
| VarEntry of ty (* type of some variable *)
| FunEntry of ty list * ty (* type of some function *)

type tenv = ty S.table
type venv = enventry S.table

type env = {venv : venv; tenv : tenv}


val empty : unit -> env
