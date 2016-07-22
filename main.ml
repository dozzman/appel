let () =
  if Array.length Sys.argv == 1 then
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.prog Lexer.token lexbuf in
    Semant.transProg Env.base_venv Env.base_tenv prog;
    print_endline (Absyn.string_of_exp prog);
  else
    let file = Sys.argv.(1) in
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    let prog = Parser.prog Lexer.token lexbuf in
    Semant.transProg Env.base_venv Env.base_tenv prog;
    print_endline (Absyn.string_of_exp prog);
    exit 0

