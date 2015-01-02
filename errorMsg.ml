open Lexing

let error_pos_string line char_start char_end = "Line: " ^ ( string_of_int line ) ^ ", characters: " ^ ( string_of_int char_start ) ^ "-" ^ ( string_of_int char_end )

let error msg lexbuf =
  let line = lexbuf.lex_curr_p.pos_lnum
  and char_start = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol
  and char_end = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    print_newline ();
    print_endline( error_pos_string line char_start char_end );
    print_endline ( "Error: " ^ msg );
    print_newline ()

let error_at msg pos_start pos_end =
  let line = pos_start.pos_lnum
  and char_start = pos_start.pos_cnum - pos_start.pos_bol
  and char_end = pos_end.pos_cnum - pos_end.pos_bol in
    print_newline ();
    print_endline ( error_pos_string line char_start char_end );
    print_endline ( "Error: " ^ msg );
    print_newline ()
