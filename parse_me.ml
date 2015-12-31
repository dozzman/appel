open Parser
open Lexer

let _ =
  let lexbuf = Lexing.from_channel stdin in
    print_string (Absyn.string_of_exp (Parser.prog Lexer.token lexbuf))

