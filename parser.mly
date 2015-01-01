
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

%token UMINUS

%nonassoc ASSIGN
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
| orTerm orableExp {}
| nonArithTerm {}
| stm {}
;

nonArithTerm:
| ID LBRACK exp RBRACK OF exp {}
| ID recdec {}
;

expTerm:
| NIL {}
| LPAREN exps RPAREN {}
| MINUS expTerm %prec UMINUS {} 
| ID LPAREN exps RPAREN {}
| lvalue {}
| NUM {}
| STRING {}
;

orableExp:
| OR orTerm orableExp {}
| {}
;

orTerm:
| andTerm andableExp {}
;

andableExp:
| AND andTerm andableExp {}
| {}
;

andTerm:
| compareTerm comparableExp {}
;

comparableExp:
| GT compareTerm comparableExp {}
| LT compareTerm comparableExp {}
| GTEQ compareTerm comparableExp {}
| LTEQ compareTerm comparableExp {}
| EQ compareTerm comparableExp {}
| NEQ compareTerm comparableExp {}
| {}
;

compareTerm:
| addTerm addableExp {}
;

addableExp:
| PLUS addTerm addableExp {}
| MINUS addTerm addableExp {}
| {}
;

addTerm:
| expTerm factorableExp {}
;

factorableExp:
| TIMES expTerm factorableExp {}
| DIV expTerm factorableExp {}
| {}
;

arithExp:
| PLUS expTerm arithExp {}
| MINUS expTerm arithExp {}
| TIMES expTerm arithExp {}
| DIV expTerm arithExp {}
| GT expTerm arithExp {}
| LT expTerm arithExp {}
| GTEQ expTerm arithExp {}
| LTEQ expTerm arithExp {}
| EQ expTerm arithExp {}
| NEQ expTerm arithExp {}
| OR expTerm arithExp {}
| AND expTerm arithExp {}
| {}
;

stm:
| ID ASSIGN exp {}
;

lvalue:
| ID {}
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
| ID COLON ID tyfieldsPrime {}
;

tyfieldsPrime:
| {}
| COMMA ID COLON ID tyfieldsPrime {}
;

recdec:
| LBRACE recfields RBRACE {}
;

recfields:
| {}
| ID EQ exp recfieldsPrime {}
;

recfieldsPrime:
| {}
| COMMA ID EQ exp recfieldsPrime {}
;
