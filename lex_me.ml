open Parser 
open Lexer

exception Eof

let token_to_string t = match t with
| TYPE -> "TYPE    "
| ARRAY -> "ARRAY    "
| OF -> "OF    "
| VAR -> "VAR    "
| NIL -> "NIL    "
| FUNCTION -> "FUNCTION    "
| LET -> "LET    "
| IN -> "IN    "
| END -> "END    "
| IF -> "IF    "
| THEN -> "THEN    "
| ELSE -> "ELSE    "
| WHILE -> "WHILE    "
| DO -> "DO    "
| FOR -> "FOR    "
| TO -> "TO    "
| BREAK -> "BREAK    "

| ID s -> "ID \"" ^ s ^ "\"    "
| NUM i -> "NUM \"" ^ (string_of_int i) ^ "\"    "
| STRING s -> "STRING \"" ^ s ^ "\"    "

| DOT -> "DOT    "
| LPAREN -> "LPAREN    "
| RPAREN -> "RPAREN    "
| LBRACE -> "LBRACE    "
| RBRACE -> "RBRACE    "
| LBRACK -> "LBRACK    "
| RBRACK -> "RBRACK    "
| SEMI -> "SEMI    "
| COLON -> "COLON    "
| COMMA -> "COMMA    "
| ASSIGN -> "ASSIGN    "

| EQ -> "EQ    "
| NEQ -> "NEQ    "
| MINUS -> "MINUS    "
| PLUS -> "PLUS    "
| TIMES -> "TIMES    "
| DIV -> "DIV    "
| GT -> "GT    "
| LT -> "LT    "
| GTEQ -> "GTEQ    "
| LTEQ -> "LTEQ    "
| AND -> "AND    "
| OR -> "OR    "
| EOF -> "EOF    "; raise Eof

let () =
  try
  let lexbuf = Lexing.from_channel stdin in
    while true do
      print_string (token_to_string (token lexbuf))
    done
  with Eof -> ()
