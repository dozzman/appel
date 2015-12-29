%{
  open Lexing
  let pos_to_string pos = "Line: " ^ ( string_of_int pos.pos_lnum ) ^ ", Character: " ^ ( string_of_int (pos.pos_cnum - pos.pos_bol))
  let eq_where_assign = ErrorMsg.error_at "expecting assign(:=) operator, did you mean (:=) instead of (=)?"
  let trailing_comma = ErrorMsg.error_at "trailing comma(,) found"
  let trailing_semi = ErrorMsg.error_at "trailing semi-colon(;) found"
  let not_a_prog = ErrorMsg.error_at "all tiger programs are expressions, are you sure you haven't started declaring variables first?"
  let unclosed_paren = ErrorMsg.error_at "unclosed parentheses found"
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

%nonassoc ASSIGN DO THEN OF
%nonassoc ELSE

%left OR
%left AND
%nonassoc GT LT GTEQ LTEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIV

%left UMINUS

%start prog

%type <unit> prog

%%

prog:
| decs EOF { print_newline(); print_endline "ACCEPT" }
| exp EOF { print_newline (); print_endline "ACCEPT" }

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
| VAR ID error { eq_where_assign $startpos $endpos }
| VAR ID COLON ID error { eq_where_assign $startpos $endpos }

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp {}
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {}

exp:
| arithExp {}
| arrayDef {}
| recDef {}
| funcall {}
| assign {}
| lvalue {}
| control {}
| LPAREN expseq RPAREN {}
| NIL {}
| NUM {}
| STRING {}

expseq:
| exp expseqMore {}
| {}

expseqMore:
| SEMI exp expseqMore {}
| {}

control:
| WHILE exp DO exp {}
| IF exp THEN exp {}
| IF exp THEN exp ELSE exp {}
| FOR lvalue ASSIGN exp TO exp DO exp {}
| FOR lvalue error { eq_where_assign $startpos $endpos }
| BREAK {}
| LET decs IN expseq END {}

assign:
| lvalue ASSIGN exp {}
| lvalue error { eq_where_assign $startpos $endpos }

arrayDef:
| ID LBRACK exp RBRACK OF exp {}

recDef:
| ID LBRACE recfields RBRACE {}

arithExp:
| MINUS exp %prec UMINUS {} 
| exp PLUS exp {}
| exp MINUS exp {}
| exp TIMES exp {}
| exp DIV exp {}
| exp GT exp {}
| exp LT exp {}
| exp GTEQ exp {}
| exp LTEQ exp {}
| exp EQ exp {}
| exp NEQ exp {}
| exp OR exp {}
| exp AND exp {}

funcall:
| ID LPAREN explist RPAREN {}

explist:
| {}
| exp explistMore {}

explistMore:
| {}
| COMMA exp explistMore {}

lvalue:
| ID deeplvalue{}

deeplvalue:
| LBRACK exp RBRACK deeplvalue {}
| DOT ID deeplvalue {}
| {}

recfields:
| ID EQ exp recfieldsMore {}
| {}

recfieldsMore:
| {}
| COMMA ID EQ exp recfieldsMore {}
