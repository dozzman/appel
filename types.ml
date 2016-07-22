type unique = unit ref

type ty = 
| INT
| STRING
| RECORD of (Symbol.symbol * ty) list * unique
| ARRAY of ty * unique
| NIL
| UNIT
| NAME of Symbol.symbol * ty option ref

let string_of_type =
  function
  | INT -> "int"
  | STRING -> "string"
  | RECORD (_,_) -> "record"
  | ARRAY (_,_)-> "array"
  | NIL -> "nil"
  | UNIT -> "unit"
  | NAME (symbol, ty) -> Symbol.name symbol
