(* Abstract Syntax Tree for Tiger *)

type symbol = Symbol.symbol
type pos = Lexing.position

type expression = {
  exp_desc: exp;
  exp_pos: pos
}

and declaration = {
  dec_desc: dec;
  dec_pos: pos
}

and type_expression = {
  ty_desc: ty;
  ty_pos: pos
}

and var_expression = {
  var_desc: var;
  var_pos: pos
}

and formal_parameter = {
  param_name: symbol;
  param_escapes: bool ref;
  param_typename: symbol;
  param_pos: pos
}

and record_label = {
  reclabel_name: symbol;
  reclabel_pos: pos
}

and var =
| SimpleVar of symbol
| FieldVar of var_expression * symbol
| SubscriptVar of var_expression * expression

and exp =
| VarExp of var_expression
| NilExp
| IntExp of int
| AssignExp of var_expression * expression (* lvalue, expression *)
| StringExp of string (* string *)
| SeqExp of expression list (* expression list *)
| CallExp of symbol * expression list (* funcion name, parameter list *)
| OpExp of expression * binop * expression (* lhs, operator, rhs *)
| RecordExp of (record_label * expression) list * symbol (* (record field, value) list, record (type) name *)
| ArrayExp of symbol * expression * expression (* name, size, initial value *)
| IfExp of expression * expression * expression option (* if test, then clause, else clause *)
| WhileExp of expression * expression (* while test, while body *)
| ForExp of symbol * bool ref * expression * expression * expression (* variable, escape, low, high, for body *)
| BreakExp
| LetExp of declaration list * expression list (* delcaration list, expression sequence *)

and dec =
| TyDec of symbol * type_expression (* type name, type *)
| VarDec of symbol * bool ref * (symbol * pos) option * expression (* var name, escape, optional type name, expression, pos *)
| FunDec of (symbol * formal_parameter list * (symbol * pos) option * expression) list
            (* (function name, formal parameter list, optional return type, body, pos) list *)

and ty = 
| NameTy of symbol (* type name *)
| RecordTy of formal_parameter list (* parameter list *)
| ArrayTy of symbol (* array type, pos *)

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

let rec string_of_var v = match v.var_desc with
| SimpleVar (symbol) -> Symbol.name symbol
| FieldVar (var, symbol) -> (string_of_var var) ^ "." ^ (Symbol.name symbol)
| SubscriptVar (var, exp) -> (string_of_var var) ^ "[" ^ (string_of_exp exp) ^ "]"

and string_of_exp exp = match exp.exp_desc with
| OpExp (lhs, op, rhs) -> "( " ^ (string_of_exp lhs) ^ " " ^ string_of_op op ^ " " ^ (string_of_exp rhs) ^ " )" 
| IntExp x -> string_of_int x
| WhileExp (test, body) -> "WHILE " ^ (string_of_exp test) ^ " DO " ^ (string_of_exp body)
| VarExp x -> string_of_var x
| NilExp -> "nil"
| StringExp s -> "\"" ^ s ^ "\""
| SeqExp x -> string_of_expseq x
| LetExp (decs, expseq) ->
  let decs_description = string_of_decs decs in
  let expseq_description = string_of_expseq expseq in
    "LET\n\t" ^ decs_description
    ^ "\nIN " ^ expseq_description ^ " END"
| _ -> "not-implemented"

and string_of_expseq = function
| [exp] -> "( " ^ string_of_exp exp ^ " )"
| expseq -> 
  let expseq_description = 
    List.fold_left 
      (fun acc (exp) ->
        acc ^ string_of_exp exp ^ "; ") 
      ""
      expseq
  in
    "( " ^ expseq_description ^ " )"

and string_of_decs decs =
  List.fold_left
    (fun acc dec ->
      (string_of_dec dec) ^ "\n\t")
    ""
    decs

and string_of_dec _ = "not-implemented"
