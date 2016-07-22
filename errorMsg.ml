open Lexing

let error_pos_string line char_start char_end = "Line: " ^ ( string_of_int line ) ^ ", characters: " ^ ( string_of_int char_start ) ^ "-" ^ ( string_of_int char_end )

let error_pos_string_small line c_pos = "Line: " ^ ( string_of_int line ) ^ ", character: " ^ ( string_of_int c_pos )

let error msg lexbuf =
  let line = lexbuf.lex_curr_p.pos_lnum in
  let char_start = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol in
  let char_end = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    print_newline ();
    print_endline( error_pos_string line char_start char_end );
    print_endline ( "Error: " ^ msg );
    print_newline ()

let error_at msg pos_start pos_end =
  let line = pos_start.pos_lnum in
  let char_start = pos_start.pos_cnum - pos_start.pos_bol in
  let char_end = pos_end.pos_cnum - pos_end.pos_bol in
    print_newline ();
    print_endline ( error_pos_string line char_start char_end );
    print_endline ( "Error: " ^ msg );
    print_newline ()

let error_at_pos msg pos =
  let line = pos.pos_lnum in
  let c_pos = pos.pos_cnum - pos.pos_bol in
    print_newline ();
    print_endline ( error_pos_string_small line c_pos );
    print_endline ( "Error: " ^ msg );
    print_newline ()

let impossible msg pos =
  let line = pos.pos_lnum in
  let c_pos = pos.pos_cnum - pos.pos_bol in
    print_newline ();
    print_endline ( error_pos_string_small line c_pos );
    print_endline ( "IMPOSSIBLE (compiler bug?): " ^ msg );
    print_newline ()
