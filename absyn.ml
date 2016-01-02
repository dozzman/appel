(* Abstract Syntax Tree for Tiger *)

type symbol = string
type pos = Lexing.position

type var =
| SimpleVar of symbol * pos
| FieldVar of var * symbol * pos
| SubscriptVar of var * exp * pos

and callexp = { func: symbol; args: exp list; pos: pos }
and opexp = { lhs: exp; op: binop; rhs: exp; pos: pos }
and recordexp = { fields: (symbol * exp * pos) list; typ: symbol; pos: pos }
and arrayexp = { typ: symbol; size: exp; init: exp; pos: pos }
and ifexp = { test: exp; thenexp: exp; elseexp: exp option; pos: pos }
and whileexp = { test: exp; body: exp; pos: pos }
and forexp = { var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp; pos: pos }
and letexp = { decs: dec list; body: (exp * pos) list; pos: pos }
and tydec = { name: symbol; ty: ty; pos: pos }
and vardec = { name: symbol; escape: bool ref; typ: (symbol * pos) option; init: exp; pos: pos }
and assignexp = { var: var; exp: exp; pos: pos }

and exp =
| VarExp of var
| NilExp
| IntExp of int
| AssignExp of assignexp
| StringExp of string * pos
| SeqExp of (exp * pos) list
| CallExp of callexp
| OpExp of opexp
| RecordExp of recordexp
| ArrayExp of arrayexp
| IfExp of ifexp
| WhileExp of whileexp
| ForExp of forexp
| BreakExp of pos
| LetExp of letexp

and dec =
| TyDec of tydec
| VarDec of vardec
| FunDec of fundec list

and ty = 
| NameTy of symbol * pos
| RecordTy of field list
| ArrayTy of symbol * pos

and fundec = { name: symbol; params: field list; result: (symbol * pos) option; body: exp; pos: pos }

and field = { name: symbol; escape: bool ref; typ: symbol; pos: pos }

and binop =
| PlusOp | MinusOp | TimesOp | DivideOp
| EqOp | NeqOp | LtOp | GtOp | LeOp | GeOp
| AndOp | OrOp

let string_of_op = function
| PlusOp -> "+"
| MinusOp -> "-"
| TimesOp -> "*"
| DivideOp -> "/"
| EqOp -> "="
| NeqOp -> "<>"
| LtOp -> "<"
| GtOp -> ">"
| LeOp -> "<="
| GeOp -> ">="
| AndOp -> "&"
| OrOp -> "|"

let rec string_of_var = function
| SimpleVar (symbol, _) -> symbol
| FieldVar (var, symbol, _) -> (string_of_var var) ^ "." ^ symbol
| SubscriptVar (var, exp, _) -> (string_of_var var) ^ "[" ^ (string_of_exp exp) ^ "]"

and string_of_exp = function
| OpExp x -> "( " ^ (string_of_exp x.lhs) ^ " " ^ string_of_op x.op ^ " " ^ (string_of_exp x.rhs) ^ " )" 
| IntExp x -> string_of_int x
| WhileExp x -> "WHILE " ^ (string_of_exp x.test) ^ " DO " ^ (string_of_exp x.body)
| VarExp x -> string_of_var x
| NilExp -> "nil"
| StringExp (s, _) -> "\"" ^ s ^ "\""
| SeqExp x -> begin match x with
  | [(exp,_)] -> "( " ^ string_of_exp exp ^ " )"
  | x -> "( " ^ (List.fold_left (fun acc (exp, _) -> acc ^ string_of_exp exp ^ "; ") "" x) ^ " )"
  end
| _ -> "not-implemented"
