
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

%nonassoc ASSIGN DO THEN
%right ELSE

%nonassoc REDUCE_ARRAY_DEF

%left OR
%left AND
%nonassoc GT LT GTEQ LTEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIV

%left UMINUS

%start main

%type <unit> main

%%

main:
| decs EOF { print_newline(); print_endline "end of file" }
;

decs:
| {}
| dec decs {}
;

dec:
| tydec {}
| vardec {}
| fundec {}
;

vardec:
| VAR ID ASSIGN exp {}
| VAR ID COLON ID ASSIGN exp {}
;

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp {}
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {}
;

exp:
| arithExp {}
| arrayDef {}
| recDef {}
| expseq {}
| funcall {}
| assign {}
| lvalue {}
| control {}
| NIL {}
| NUM {}
| STRING {}
;

control:
| WHILE exp DO exp {}
| IF exp THEN exp {}
| IF exp THEN exp ELSE exp {}
| FOR ASSIGN exp TO exp DO exp {}
| BREAK {}
| LET decs IN expseq END {}
;

assign:
| lvalue ASSIGN exp {}
;

arrayDef:
| lvalue LBRACK exp RBRACK OF exp %prec REDUCE_ARRAY_DEF {}
;

recDef:
| ID recdec {}
;

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
;

funcall:
| ID LPAREN exps RPAREN {}
;

lvalue:
| ID {}
| lvalue DOT ID {}
| lvalue LBRACK exp RBRACK {}
;

expseq:
| LPAREN exps RPAREN {}
;

exps:
| {}
| exp {}
| exps SEMI exp {}
;

tydec:
| TYPE ID EQ ty {}
;

ty:
| ID {}
| LBRACE tyfields RBRACE {}
| ARRAY OF ID {}
;

tyfields:
| {}
| ID COLON ID tyfieldsMore {}
;

tyfieldsMore:
| {}
| COMMA ID COLON ID tyfieldsMore {}
;

recdec:
| LBRACE recfields RBRACE {}
;

recfields:
| {}
| ID EQ exp recfieldsMore {}
;

recfieldsMore:
| {}
| COMMA ID EQ exp recfieldsMore {}
;
