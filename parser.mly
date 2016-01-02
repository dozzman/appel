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
| TYPE ID EQ ty { TyDec {name = $2; ty = $4; pos = $startpos} }

ty:
| ID { NameTy ($1, $startpos) }
| LBRACE tyfields RBRACE { RecordTy $2 }
| ARRAY OF ID { ArrayTy ($3, $startpos) }

tyfields:
| { [] }
| ID COLON ID tyfieldsMore { {name = $1; escape = global_escape; typ = $3; pos = $startpos} :: $4 }

tyfieldsMore:
| { [] }
| COMMA ID COLON ID tyfieldsMore { {name = $2; escape = global_escape; typ = $4; pos = $startpos($2)} :: $5 }

vardec:
| VAR ID ASSIGN exp { VarDec {name = $2; escape = global_escape; typ = None; init = $4; pos = $startpos} }
| VAR ID COLON ID ASSIGN exp { VarDec {name = $2; escape = global_escape; typ = Some ($4, $startpos($4)); init = $6; pos = $startpos} }
| VAR ID error exp { eq_where_assign $startpos $endpos; VarDec {name = $2; escape = global_escape; typ = None; init = $4; pos = $startpos} }
| VAR ID COLON ID error exp { eq_where_assign $startpos $endpos; VarDec {name = $2; escape = global_escape; typ = Some ($4, $startpos($4)); init = $6; pos = $startpos} }

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp fundecMore { {name = $2; params = $4; result = None; body = $7; pos = $startpos} :: $8 }
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundecMore { {name = $2; params = $4; result = Some ($7, $startpos($7)); body = $9; pos = $startpos} :: $10 }

fundecMore:
| AND FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec { {name = $3; params = $5; result = None; body = $8; pos = $startpos} :: $9 }
| AND FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec { {name = $3; params = $5; result = Some ($8, $startpos($8)); body = $10; pos = $startpos} :: $11 }
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
| WHILE exp DO exp { WhileExp {test = $2; body = $4; pos = $startpos} }
| IF exp THEN exp { IfExp {test = $2; thenexp = $4; elseexp = None; pos = $startpos} }
| IF exp THEN exp ELSE exp { IfExp {test = $2; thenexp = $4; elseexp = Some $6; pos = $startpos} }
| FOR ID ASSIGN exp TO exp DO exp { ForExp {var = $2;  escape = global_escape; lo = $4; hi = $6; body = $8; pos = $startpos} }
| FOR ID error { eq_where_assign $startpos $endpos; ForExp {var = $2;  escape = global_escape; lo = IntExp 0; hi = IntExp 0; body = IntExp 0; pos = $startpos} }
| BREAK {BreakExp $startpos}
| LET decs IN expseq END { LetExp {decs = $2; body = $4; pos = $startpos} }

assign:
| lvalue ASSIGN exp { AssignExp {var = $1; exp = $3; pos = $startpos($2)} }
| lvalue error { eq_where_assign $startpos $endpos; AssignExp {var = $1; exp = IntExp 0; pos = $startpos} }

arrayDef:
| ID LBRACK exp RBRACK OF exp { ArrayExp {typ = $1; size = $3; init = $6; pos = $startpos} }

recDef:
| ID LBRACE recfields RBRACE { RecordExp {fields = $3; typ = $1; pos = $startpos} }

arithExp:
| MINUS exp %prec UMINUS { OpExp {lhs = IntExp 0; op = MinusOp; rhs = $2; pos = $startpos } }
| exp PLUS exp { OpExp {lhs = $1; op = PlusOp; rhs = $3; pos = $startpos($2) } }
| exp MINUS exp { OpExp {lhs = $1; op = MinusOp; rhs = $3; pos = $startpos($2) } }
| exp TIMES exp { OpExp {lhs = $1; op = TimesOp; rhs = $3; pos = $startpos($2) } }
| exp DIV exp { OpExp {lhs = $1; op = DivideOp; rhs = $3; pos = $startpos($2) } }
| exp GT exp { OpExp {lhs = $1; op = GtOp; rhs = $3; pos = $startpos($2) } }
| exp LT exp { OpExp {lhs = $1; op = LtOp; rhs = $3; pos = $startpos($2) } }
| exp GTEQ exp { OpExp {lhs = $1; op = GeOp; rhs = $3; pos = $startpos($2) } }
| exp LTEQ exp { OpExp {lhs = $1; op = LeOp; rhs = $3; pos = $startpos($2) } }
| exp EQ exp { OpExp {lhs = $1; op = EqOp; rhs = $3; pos = $startpos($2) } }
| exp NEQ exp { OpExp {lhs = $1; op = NeqOp; rhs = $3; pos = $startpos($2) } }
| exp OR exp { OpExp {lhs = $1; op = OrOp; rhs = $3; pos = $startpos($2) } }
| exp AND exp { OpExp {lhs = $1; op = AndOp; rhs = $3; pos = $startpos($2) } }

funcall:
| ID LPAREN explist RPAREN { CallExp {func = $1; args = $3; pos = $startpos} }

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
