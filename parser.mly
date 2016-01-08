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
%nonassoc ASSIGN
%nonassoc OF
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
| exp EOF { $1 }

decs:
| {[]}
| dec decs { $1 :: $2 }

dec:
| tydec { $1 }
| vardec { $1 }
| fundec { FunDec $1 }

tydec:
| TYPE ID EQ ty { TyDec ($2, $4, $startpos) }

ty:
| ID { NameTy ($1, $startpos) }
| LBRACE tyfields RBRACE { RecordTy $2 }
| ARRAY OF ID { ArrayTy ($3, $startpos) }

tyfields:
| { [] }
| ID COLON ID tyfieldsMore { ($1, global_escape, $3, $startpos) :: $4 }

tyfieldsMore:
| { [] }
| COMMA ID COLON ID tyfieldsMore { ($2, global_escape, $4, $startpos($2)) :: $5 }

vardec:
| VAR ID ASSIGN exp { VarDec ($2, global_escape, None, $4, $startpos) }
| VAR ID COLON ID ASSIGN exp { VarDec ($2, global_escape, Some ($4, $startpos($4)), $6, $startpos) }
| VAR ID error exp { eq_where_assign $startpos $endpos; VarDec ($2, global_escape, None, $4, $startpos) }
| VAR ID COLON ID error exp { eq_where_assign $startpos $endpos; VarDec ($2, global_escape, Some ($4, $startpos($4)), $6, $startpos) }

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp fundecMore { ($2, $4, None, $7, $startpos) :: $8 }
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundecMore { ($2, $4, Some ($7, $startpos($7)), $9, $startpos) :: $10 }

fundecMore:
| AND FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec { ($3, $5, None, $8, $startpos) :: $9 }
| AND FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec { ($3, $5, Some ($8, $startpos($8)), $10, $startpos) :: $11 }
| { [] }

exp:
| arithExp { $1 }
| arrayDef { $1 }
| recDef { $1 }
| funcall { $1 }
| assign { $1 }
| LPAREN expseq RPAREN { SeqExp $2 }
| lvalue { VarExp $1 }
| control { $1 }
| NIL { NilExp }
| NUM { IntExp $1 }
| STRING { StringExp ($1, $startpos) }

expseq:
| exp expseqMore { ($1, $startpos) :: $2 }
| {[]}

expseqMore:
| SEMI exp expseqMore { ($2, $startpos($2)) :: $3 }
| {[]}


control:
| WHILE exp DO exp { WhileExp ($2, $4, $startpos) }
| IF exp THEN exp { IfExp ($2, $4, None, $startpos) }
| IF exp THEN exp ELSE exp { IfExp ($2, $4, Some $6, $startpos) }
| FOR ID ASSIGN exp TO exp DO exp { ForExp ($2,  global_escape, $4, $6, $8, $startpos) }
| FOR ID error { eq_where_assign $startpos $endpos; ForExp ($2,  global_escape, IntExp 0, IntExp 0, IntExp 0, $startpos) }
| BREAK {BreakExp $startpos}
| LET decs IN expseq END { LetExp ($2, $4, $startpos) }

assign:
| lvalue ASSIGN exp { AssignExp ($1, $3, $startpos($2)) }
| lvalue error { eq_where_assign $startpos $endpos; AssignExp ($1, IntExp 0, $startpos) }

arrayDef:
| ID LBRACK exp RBRACK OF exp { ArrayExp ($1, $3, $6, $startpos) }

recDef:
| ID LBRACE recfields RBRACE { RecordExp ($3, $1, $startpos) }

arithExp:
| MINUS exp %prec UMINUS { OpExp (IntExp 0, MinusOp, $2, $startpos ) }
| exp PLUS exp { OpExp ($1, PlusOp, $3, $startpos($2) ) }
| exp MINUS exp { OpExp ($1, MinusOp, $3, $startpos($2) ) }
| exp TIMES exp { OpExp ($1, TimesOp, $3, $startpos($2) ) }
| exp DIV exp { OpExp ($1, DivideOp, $3, $startpos($2) ) }
| exp GT exp { OpExp ($1, GtOp, $3, $startpos($2) ) }
| exp LT exp { OpExp ($1, LtOp, $3, $startpos($2) ) }
| exp GTEQ exp { OpExp ($1, GeOp, $3, $startpos($2) ) }
| exp LTEQ exp { OpExp ($1, LeOp, $3, $startpos($2) ) }
| exp EQ exp { OpExp ($1, EqOp, $3, $startpos($2) ) }
| exp NEQ exp { OpExp ($1, NeqOp, $3, $startpos($2) ) }
| exp OR exp { OpExp ($1, OrOp, $3, $startpos($2) ) }
| exp AND exp { OpExp ($1, AndOp, $3, $startpos($2) ) }

funcall:
| ID LPAREN explist RPAREN { CallExp ($1, $3, $startpos) }

explist:
| { [] }
| exp explistMore { $1 :: $2 }

explistMore:
| { [] }
| COMMA exp explistMore { $2 :: $3 }

lvalue:
| subscriptLvalue { $1 }
| dotLvalue { $1 }
| ID { SimpleVar ($1, $startpos) }

dotLvalue:
| ID DOT ID { FieldVar (SimpleVar ($1, $startpos), $3, $startpos($3)) }
| subscriptLvalue DOT ID { FieldVar ($1, $3, $startpos($3)) }
| dotLvalue DOT ID { FieldVar ($1, $3, $startpos($3)) }

subscriptLvalue:
| ID LBRACK exp RBRACK { SubscriptVar (SimpleVar ($1, $startpos), $3, $startpos($3)) }
| subscriptLvalue LBRACK exp RBRACK { SubscriptVar ($1, $3, $startpos($3)) }
| dotLvalue LBRACK exp RBRACK { SubscriptVar ($1, $3, $startpos($3)) }

recfields:
| { [] }
| ID EQ exp recfieldsMore { ($1, $3, $startpos) :: $4 }

recfieldsMore:
| { [] }
| COMMA ID EQ exp recfieldsMore { ($2, $4, $startpos($2)) :: $5 }
