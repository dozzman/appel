
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


%left OR
%left AND
%nonassoc GT LT GTEQ LTEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIV

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
| FUNCTION ID LPAREN tyfields RPAREN EQ progExp {}
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ progExp {}
;

progExp:
| exp {}
| stm {}
;

exp:
| arithExp {}
| nonArithTerm {}
;

nonArithTerm:
| lvalue LBRACK exp RBRACK OF exp {}
| ID recdec {}
;

expTerm:
| NIL {}
| LPAREN progExps RPAREN {}
| MINUS arithExp {} 
| ID LPAREN progExps RPAREN {}
| lvalue {}
| NUM {}
| STRING {}
;

arithExp:
| arithExp PLUS arithExp {}
| arithExp MINUS arithExp {}
| arithExp TIMES arithExp {}
| arithExp DIV arithExp {}
| arithExp GT arithExp {}
| arithExp LT arithExp {}
| arithExp GTEQ arithExp {}
| arithExp LTEQ arithExp {}
| arithExp EQ arithExp {}
| arithExp NEQ arithExp {}
| arithExp OR arithExp {}
| arithExp AND arithExp {}
| expTerm {}
;

stm:
| lvalue ASSIGN exp {}
| IF exp THEN progExp {}
;

lvalue:
| ID {}
| lvalue DOT ID {}
| lvalue LBRACK exp RBRACK {}
;

progExps:
| {}
| progExp {}
| progExps SEMI progExp {}
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
| ID EQ progExp recfieldsMore {}
;

recfieldsMore:
| {}
| COMMA ID EQ progExp recfieldsMore {}
;
