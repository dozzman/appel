open Lexing

let error msg lexbuf =
  let line = string_of_int lexbuf.lex_curr_p.pos_lnum
  and char_start = string_of_int (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
  and char_end = string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol) in
    print_newline ();
    print_endline(
      "Line: " ^ line ^  
      ", characters: " ^ char_start ^ "-" ^ char_end
    );
    print_endline ( "Error: " ^ msg );
    print_newline ()
