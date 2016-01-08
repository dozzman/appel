(* Abstract Syntax Tree for Tiger *)

type symbol = string
type pos = Lexing.position

type var =
| SimpleVar of symbol * pos
| FieldVar of var * symbol * pos
| SubscriptVar of var * exp * pos

and exp =
| VarExp of var
| NilExp
| IntExp of int
| AssignExp of var * exp * pos (* lvalue, expression, pos *)
| StringExp of string * pos (* string, pos *)
| SeqExp of (exp * pos) list (* (expression, pos) list *)
| CallExp of symbol * exp list * pos (* funcion, parameter list, pos *)
| OpExp of exp * binop * exp * pos (* lhs, operator, rhs, pos *)
| RecordExp of (symbol * exp * pos) list * symbol * pos (* (record field, value, pos) list, record (type) name, pos *)
| ArrayExp of symbol * exp * exp * pos (* name, size, initial value, pos *)
| IfExp of exp * exp * exp option * pos (* if test, then clause, else clause, pos *)
| WhileExp of exp * exp * pos (* while test, while body, pos *)
| ForExp of symbol * bool ref * exp * exp * exp * pos (* variable, escape, low, high, for body, pos *)
| BreakExp of pos
| LetExp of dec list * (exp * pos) list * pos (* delcaration list, expression sequence, pos *)

and dec =
| TyDec of symbol * ty * pos (* type name, type, pos *)
| VarDec of symbol * bool ref * (symbol * pos) option * exp * pos (* var name, escape, optional type name, expression, pos *)
| FunDec of (symbol * (symbol * bool ref * symbol * pos) list * (symbol * pos) option * exp * pos) list
            (* (function name, (field name, escape, type name, pos) field list, optional return type, body, pos) list *)

and ty = 
| NameTy of symbol * pos (* type name, pos *)
| RecordTy of (symbol * bool ref * symbol * pos) list (* field name, escape, field type, pos *)
| ArrayTy of symbol * pos (* array type, pos *)

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
| OpExp (lhs, op, rhs, _) -> "( " ^ (string_of_exp lhs) ^ " " ^ string_of_op op ^ " " ^ (string_of_exp rhs) ^ " )" 
| IntExp x -> string_of_int x
| WhileExp (test, body, pos) -> "WHILE " ^ (string_of_exp test) ^ " DO " ^ (string_of_exp body)
| VarExp x -> string_of_var x
| NilExp -> "nil"
| StringExp (s, _) -> "\"" ^ s ^ "\""
| SeqExp x -> begin match x with
  | [(exp,_)] -> "( " ^ string_of_exp exp ^ " )"
  | x -> "( " ^ (List.fold_left (fun acc (exp, _) -> acc ^ string_of_exp exp ^ "; ") "" x) ^ " )"
  end
| _ -> "not-implemented"
