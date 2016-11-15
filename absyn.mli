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

and fun_expression = {
  formals: formal_parameter list;
  ret_ty: (symbol * pos) option;
  body: expression;
  fun_pos: pos;
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
| LetExp of declaration list * expression (* delcaration list, expression sequence *)

and dec =
| TyDec of (symbol * type_expression) list (* type name, type *)
| VarDec of symbol * bool ref * (symbol * pos) option * expression (* var name, escape, optional type name, expression, pos *)
| FunDec of (symbol * fun_expression) list (* (function name, formal parameter list, optional return type, body) list *)

and ty = 
| NameTy of symbol (* type name *)
| RecordTy of formal_parameter list (* parameter list *)
| ArrayTy of symbol (* array type, pos *)

and binop =
| PlusOp | MinusOp | TimesOp | DivideOp
| EqOp | NeqOp | LtOp | GtOp | LeOp | GeOp
| AndOp | OrOp

val string_of_exp : expression -> string
