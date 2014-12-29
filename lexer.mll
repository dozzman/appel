{
  open Parser
  exception Eof
  exception Unexpected_char_error
  exception Unclosed_string
  exception Unexpected_end_of_file
  exception Unmatched_end_of_comment
  exception Unclosed_comment
  let comment_counter = ref 0
}

let ch = [ 'a' - 'z' 'A' - 'Z' ]
let digit = [ '0' - '9' ]
let id = ch ( ch | digit )* 

rule token = parse
| [ ' ' '\t' ] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "do" { DO }
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

| "/*" { comment_counter := !comment_counter + 1; comment lexbuf }
| "*/" {
  ErrorMsg.error "Potentially unmatched end of comment found" lexbuf;
  raise Unmatched_end_of_comment
}

| eof { raise Eof }

| _ as lxbad { 
  if Lexing.lexeme_char lexbuf 0 = '"' then
  begin
    ErrorMsg.error "Potentially unclosed string found" lexbuf;
    raise Unclosed_string
  end
  else
  begin
    ErrorMsg.error ("Unexpected character: '" ^ String.make 1 lxbad ^ "'" ) lexbuf;
    raise Unexpected_char_error
  end
}

and comment = parse
| "/*" { comment_counter := !comment_counter + 1; comment lexbuf }
| "*/" {
  comment_counter := !comment_counter - 1;
  if !comment_counter > 0 then
    comment lexbuf
  else
    token lexbuf
}

| eof {
  ErrorMsg.error "Potentially Unclosed comment found" lexbuf;
  raise Unclosed_comment
}
| _ { comment lexbuf }
