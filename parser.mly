%{
  open Lexing
  let pos_to_string pos = "Line: " ^ ( string_of_int pos.pos_lnum ) ^ ", Character: " ^ ( string_of_int (pos.pos_cnum - pos.pos_bol))
  let eq_where_assign = ErrorMsg.error_at "expecting assign(:=) operator, did you mean (:=) instead of (=)?"
  let trailing_comma = ErrorMsg.error_at "trailing comma(,) found"
  let trailing_semi = ErrorMsg.error_at "trailing semi-colon(;) found"
  let unclosed_paren = ErrorMsg.error_at "unclosed parentheses found"
  open Absyn

  let global_escape = ref true
%}

%token TYPE
%token ARRAY 
%token OF 
%token VAR 
%token NIL 
%token FUNCTION 
%token LET 
%token IN 
%token END 
%token IF 
%token THEN 
%token ELSE 
%token WHILE 
%token DO 
%token FOR 
%token TO 
%token BREAK 

%token <string> ID 
%token <int> NUM 
%token <string> STRING 

%token DOT 
%token LPAREN 
%token RPAREN 
%token LBRACE 
%token RBRACE 
%token LBRACK 
%token RBRACK 
%token SEMI 
%token COLON 
%token COMMA 
%token ASSIGN 

%token EQ 
%token NEQ 
%token MINUS 
%token PLUS 
%token TIMES 
%token DIV 
%token GT 
%token LT 
%token GTEQ 
%token LTEQ 
%token AND 
%token OR 
%token EOF 

%nonassoc DO THEN
(* not useful anymore apparently *)
(*%nonassoc ASSIGN OF *)
%nonassoc ELSE

%left OR
%left AND
%nonassoc GT LT GTEQ LTEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIV

%left UMINUS

%start prog

%type <Absyn.exp> prog

%%

prog:
| decs EOF { NilExp }
| exp EOF { $1 }

decs:
| {}
| dec decs {}

dec:
| tydec {}
| vardec {}
| fundec {}

tydec:
| TYPE ID EQ ty {}

ty:
| ID {}
| LBRACE tyfields RBRACE {}
| ARRAY OF ID {}

tyfields:
| {}
| ID COLON ID tyfieldsMore {}

tyfieldsMore:
| {}
| COMMA ID COLON ID tyfieldsMore {}

vardec:
| VAR ID ASSIGN exp {}
| VAR ID COLON ID ASSIGN exp {}
| VAR ID error exp { eq_where_assign $startpos $endpos }
| VAR ID COLON ID error exp { eq_where_assign $startpos $endpos }

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp {}
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {}

exp:
| arithExp { $1 }
(*| arrayDef {}
| recDef {}
| funcall {}
| assign {}*)
| LPAREN expseq RPAREN  { $2 }
| lvalue { VarExp $1 }
| control { $1 }
| NIL { NilExp }
| NUM { IntExp $1 }
| STRING { StringExp ($1, $startpos) }

expseq:
| exp expseqMore { match $2 with SeqExp l -> SeqExp (($1, $startpos) :: l) | _ -> raise Parsing.Parse_error }
| {SeqExp []}

expseqMore:
| SEMI exp expseqMore { match $3 with SeqExp l -> SeqExp (($2, $startpos) :: l) | _ -> raise Parsing.Parse_error }
| {SeqExp []}

control:
| WHILE exp DO exp { WhileExp {test = $2; body = $4; pos = $startpos} }
| IF exp THEN exp { IfExp {test = $2; thenexp = $4; elseexp = None; pos = $startpos} }
| IF exp THEN exp ELSE exp { IfExp {test = $2; thenexp = $4; elseexp = Some $6; pos = $startpos} }
| FOR ID ASSIGN exp TO exp DO exp { ForExp {var = $2;  escape = global_escape; lo = $4; hi = $6; body = $8; pos = $startpos} }
| FOR ID error exp TO exp DO exp { eq_where_assign $startpos $endpos; ForExp {var = $2;  escape = global_escape; lo = $4; hi = $6; body = $8; pos = $startpos} }
| BREAK {BreakExp $startpos}
(*| LET decs IN expseq END {LetExp}*)

assign:
| lvalue ASSIGN exp {}
| lvalue error { eq_where_assign $startpos $endpos }

arrayDef:
| ID LBRACK exp RBRACK OF exp {}

recDef:
| ID LBRACE recfields RBRACE {}

arithExp:
| MINUS exp %prec UMINUS { OpExp {lhs = IntExp 0; op = MinusOp; rhs = $2; pos = $startpos } }
| exp PLUS exp { OpExp {lhs = $1; op = PlusOp; rhs = $3; pos = $startpos } }
| exp MINUS exp { OpExp {lhs = $1; op = MinusOp; rhs = $3; pos = $startpos } }
| exp TIMES exp { OpExp {lhs = $1; op = TimesOp; rhs = $3; pos = $startpos } }
| exp DIV exp { OpExp {lhs = $1; op = DivideOp; rhs = $3; pos = $startpos } }
| exp GT exp { OpExp {lhs = $1; op = GtOp; rhs = $3; pos = $startpos } }
| exp LT exp { OpExp {lhs = $1; op = LtOp; rhs = $3; pos = $startpos } }
| exp GTEQ exp { OpExp {lhs = $1; op = GeOp; rhs = $3; pos = $startpos } }
| exp LTEQ exp { OpExp {lhs = $1; op = LeOp; rhs = $3; pos = $startpos } }
| exp EQ exp { OpExp {lhs = $1; op = EqOp; rhs = $3; pos = $startpos } }
| exp NEQ exp { OpExp {lhs = $1; op = NeqOp; rhs = $3; pos = $startpos } }
| exp OR exp { OpExp {lhs = $1; op = OrOp; rhs = $3; pos = $startpos } }
| exp AND exp { OpExp {lhs = $1; op = AndOp; rhs = $3; pos = $startpos } }

funcall:
| ID LPAREN explist RPAREN {}

explist:
| {}
| exp explistMore {}

explistMore:
| {}
| COMMA exp explistMore {}

lvalue:
| lvalue LBRACK exp RBRACK { SubscriptVar ($1, $3, $startpos) }
| lvalue DOT ID { FieldVar ($1, $3, $startpos) }
| ID { SimpleVar ($1, $startpos) }

recfields:
| ID EQ exp recfieldsMore {}
| {}

recfieldsMore:
| {}
| COMMA ID EQ exp recfieldsMore {}
