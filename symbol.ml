type symbol = string * int

let hashtable : (string, int) Hashtbl.t = Hashtbl.create 128
let next_symbol = ref 0

let symbol s =
  try
    (s, Hashtbl.find hashtable s)
  with Not_found ->
    let sym = !next_symbol in
    begin
      Hashtbl.add hashtable s sym;
      next_symbol := sym + 1;
      (s, sym)
    end

let name (s,n) = s

module IntOrderedType =
struct
  type t = int
  let compare x1 x2 = x1 - x2
end

let compare (s1,n1) (s2,n2) = (IntOrderedType.compare n1 n2) = 0

module IntMap = Map.Make(IntOrderedType)
type 'a table = 'a IntMap.t
let empty = IntMap.empty;;

let enter table ((s,n) : symbol) value = IntMap.add n value table
let look table ((s,n) : symbol) =
  try
    Some (IntMap.find n table)
  with Not_found ->
    None
