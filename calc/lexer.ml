type ('a, 'b) state_machine = { id : 'b; mutable edges : ('a, 'b) state_transition list }

and ('a, 'b) state_transition = 'a * ('a, 'b) state_machine

type token = 
| IF
| LPAREN
| RPAREN

type lexer_return = 
| Final of token
| Node

type lexer_machine = (char, lexer_return) state_machine

let even_1s = 
  let even_state = { id = true; edges = [] }
  and odd_state = { id = false; edges = [] } in
   even_state.edges <- ('0', even_state)::even_state.edges;
   even_state.edges <- ('1', odd_state)::even_state.edges;
   odd_state.edges <- ('0', odd_state)::odd_state.edges;
   odd_state.edges <- ('1', even_state)::odd_state.edges;
   even_state

let parse stream machine = 
  let curr_state = ref machine in
    let next_state () =
      let v = Stream.next stream in
        match List.find ( fun (v1, _) -> v1 = v ) !curr_state.edges with
        | ( v1, s ) -> s
    in
      try
        while true do
            curr_state := (next_state ())
        done;
        (!curr_state).id
      with
      | Stream.Failure -> (!curr_state).id
      | Not_found -> raise Not_found (* at this point, need to return final state with longest match *)

let _ =
  let in_stream = Stream.of_channel stdin in
    let result = parse in_stream even_1s in
      print_newline ();
      if result then print_endline "true" else print_endline "false"
