open Lexing

val error : string -> lexbuf -> unit
val error_at : string -> position -> position -> unit
val error_at_pos : string -> position -> unit
val impossible : string -> position -> unit
