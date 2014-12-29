type id = string

type binop = Plus | Minus | Times | Div

type stm =
  CompoundStm of stm * stm
| AssignStm of id * exp
| PrintStm of exp list

and exp =
  IdExp of id
| NumExp of int
| OpExp of exp * binop * exp
| EseqExp of stm * exp

exception Id_not_found of id

let update i v vlist = (i, v)::vlist

let rec lookup i vlist = match vlist with
| (i1, v1)::vrest -> if i = i1 then v1 else lookup i vrest
| [] -> raise (Id_not_found i)

let rec maxargs s = match s with
| CompoundStm ( s1, s2 ) -> max ( maxargs s1 ) ( maxargs s2 )
| PrintStm elist -> max (List.length elist) (List.fold_left ( fun total e1 -> max total (maxargsExp e1) ) 0 elist )
| AssignStm ( _, e )-> maxargsExp e

and maxargsExp e = match e with
| IdExp _ | NumExp _ -> 0
| OpExp ( e1, op1, e2 ) -> max (maxargsExp e1) (maxargsExp e2)
| EseqExp ( s1, e1 ) -> max (maxargs s1) (maxargsExp e1)

let rec interpStm s vlist = match s with
| CompoundStm ( s1, s2 ) -> interpStm s2 (interpStm s1 vlist)
| AssignStm ( i, e ) -> 
  let ( v, vlist2 ) = interpExp e vlist in
    update i v vlist2
| PrintStm ( elist ) -> let vlist1 = (List.fold_left ( fun tbl e -> let ( v, vlist1 ) = interpExp e tbl in print_string ((string_of_int v) ^ " "); vlist1 ) vlist elist) in print_newline (); vlist1

and interpExp e vlist = match e with 
| IdExp i -> (lookup i vlist, vlist)
| NumExp v -> (v, vlist)
| OpExp ( e1, op1, e2 ) ->
  let (v1, vlist1) = interpExp e1 vlist in
    let (v2, vlist2) = interpExp e2 vlist1 in
    begin
      match op1 with
      | Plus -> (v1 + v2, vlist2)
      | Minus -> (v1 - v2, vlist2)
      | Times -> (v1 * v2, vlist2)
      | Div -> (v1 / v2, vlist2)
    end
| EseqExp ( s1, e1 ) -> interpExp e1 (interpStm s1 vlist)
    

let prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a";OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))


let prog2 = PrintStm[IdExp"a";OpExp(IdExp"a", Minus,NumExp 1)] 

let () = List.iter (fun (i,v) -> print_endline (i ^ " |-> " ^ (string_of_int v))) (interpStm prog []); print_newline ()
