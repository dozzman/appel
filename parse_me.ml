open Parser
open Lexer

let _ =
  let lexbuf = Lexing.from_channel stdin in
    ignore (Parser.main Lexer.token lexbuf)
