{
  open Parser
  exception Eof
  exception Unexpected_char_error
}

let ch = [ 'a' - 'z' 'A' - 'Z' ]
let digit = [ '0' - '9' ]
let id = ch ( ch | digit )+ 
rule token = parse
| [ ' ' '\t' '\n' ] { token lexbuf }
| "if" { IF }
| "in" { IN }
| "of" { OF }
| "to" { TO }
| "end" { END }
| "for" { FOR }
| "let" { LET }
| "nil" { NIL }
| "var" { VAR }
| "else" { ELSE }
| "then" { THEN }
| "type" { TYPE }
| "array" { ARRAY }
| "break" { BREAK }
| "while" { WHILE }
| "function" { FUNCTION }

| id as lxid { ID( lxid ) }
| digit+ as lxnum { NUM( int_of_string lxnum ) }

| '"' ( [ ^ '"' ]+ as lxstring ) '"' { STRING( lxstring ) } 

| '.' { DOT }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACK }
| ']' { RBRACK }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }
| ":=" { ASSIGN }

| '=' { EQ }
| "<>" { NEQ }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }
| '>' { GT }
| '<' { LT }
| ">=" { GTEQ }
| "<=" { LTEQ }
| '&' { AND }
| '|' { OR }
| _ as lxbad { print_endline ( "unexpected character '" ^ ( String.make 1 lxbad) ^ "'" ) ; raise Unexpected_char_error }
| eof { raise Eof } 
