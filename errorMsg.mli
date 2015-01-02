open Lexing

val error : string -> lexbuf -> unit
val error_at : string -> position -> position -> unit
