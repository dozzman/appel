%{
  open Lexing
  open Absyn
  module S = Symbol

  let pos_to_string pos = "Line: " ^ ( string_of_int pos.pos_lnum ) ^ ", Character: " ^ ( string_of_int (pos.pos_cnum - pos.pos_bol))
  let eq_where_assign = ErrorMsg.error_at "expecting assign(:=) operator, did you mean (:=) instead of (=)?"
  let trailing_comma = ErrorMsg.error_at "trailing comma(,) found"
  let trailing_semi = ErrorMsg.error_at "trailing semi-colon(;) found"
  let unclosed_paren = ErrorMsg.error_at "unclosed parentheses found"
  let badly_formed_record_definition = ErrorMsg.error_at "badly formed record definition"
  let missing_variable_name = ErrorMsg.error_at "missing variable name"

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

%type <Absyn.expression> prog

%%

prog:
| exp EOF { $1 }

decs:
| {[]}
| dec decs { $1 :: $2 }

dec:
| tydec { $1 }
| vardec { $1 }
| fundec { { dec_desc = FunDec $1;
             dec_pos = $startpos } }

tydec:
| TYPE ID EQ ty { { dec_desc = TyDec (S.symbol $2, $4);
                    dec_pos = $startpos } }

ty:
| ID { { ty_desc = NameTy (S.symbol $1);
         ty_pos = $startpos } }
| LBRACE tyfields RBRACE { { ty_desc = RecordTy $2;
                             ty_pos = $startpos } }
| ARRAY OF ID { { ty_desc = ArrayTy (S.symbol $3);
                  ty_pos = $startpos } }

tyfields:
| { [] }
| ID COLON ID tyfieldsMore { { param_name = S.symbol $1;
                             param_escapes = global_escape;
                             param_typename = S.symbol $3;
                             param_pos = $startpos} :: $4 }

tyfieldsMore:
| { [] }
| COMMA ID COLON ID tyfieldsMore { { param_name = S.symbol $2;
                                     param_escapes = global_escape;
                                     param_typename = S.symbol $4;
                                     param_pos = $startpos($2)} :: $5 }

vardec:
| VAR ID ASSIGN exp { { dec_desc = VarDec (S.symbol $2, global_escape, None, $4);
                        dec_pos = $startpos } }
| VAR ID COLON ID ASSIGN exp { { dec_desc = VarDec (S.symbol $2, global_escape, Some (S.symbol $4, $startpos($4)), $6);
                                 dec_pos = $startpos } }
| VAR ID error exp { eq_where_assign $startpos $endpos;
                     { dec_desc = VarDec (S.symbol $2, global_escape, None, $4);
                       dec_pos = $startpos } }
| VAR ID COLON ID error exp { eq_where_assign $startpos $endpos;
                              { dec_desc = VarDec (S.symbol $2, global_escape, Some (S.symbol $4, $startpos($4)), $6);
                                dec_pos = $startpos } }
| VAR ASSIGN exp { missing_variable_name $startpos $endpos;
                   { dec_desc = VarDec (S.symbol "undef", global_escape, None, $3);
                     dec_pos = $startpos } }
| VAR COLON ID ASSIGN exp { { dec_desc = VarDec (S.symbol "undef", global_escape, Some (S.symbol $3, $startpos($3)), $5);
                              dec_pos = $startpos } }

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp fundecMore { (S.symbol $2, $4, None, $7) :: $8 }
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundecMore { (S.symbol $2, $4, Some (S.symbol $7, $startpos($7)), $9) :: $10 }

fundecMore:
| AND FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec { (S.symbol $3, $5, None, $8) :: $9 }
| AND FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec { (S.symbol $3, $5, Some (S.symbol $8, $startpos($8)), $10) :: $11 }
| { [] }

exp:
| arithExp { $1 }
| arrayDef { $1 }
| recDef { $1 }
| funcall { $1 }
| assign { $1 }
| LPAREN expseq RPAREN { { exp_desc = SeqExp $2;
                           exp_pos = $startpos } }
| lvalue { { exp_desc = VarExp $1;
             exp_pos = $startpos } }

| control { $1 }
| NIL { { exp_desc = NilExp; exp_pos = $startpos } }
| NUM { { exp_desc = IntExp $1; exp_pos = $startpos } }
| STRING { { exp_desc = StringExp $1; exp_pos = $startpos } }

expseq:
| exp expseqMore { $1 :: $2 }
| {[]}

expseqMore:
| SEMI exp expseqMore { $2 :: $3 }
| SEMI { trailing_semi $startpos $endpos; [] }
| {[]}


control:
| WHILE exp DO exp { { exp_desc = WhileExp ($2, $4);
                       exp_pos = $startpos } }
| IF exp THEN exp { { exp_desc = IfExp ($2, $4, None);
                      exp_pos = $startpos } }
| IF exp THEN exp ELSE exp { { exp_desc = IfExp ($2, $4, Some $6);
                               exp_pos = $startpos } }
| FOR ID ASSIGN exp TO exp DO exp { { exp_desc = ForExp (S.symbol $2,  global_escape, $4, $6, $8);
                                      exp_pos =  $startpos } }
| FOR ID error { eq_where_assign $startpos $endpos;
                 { exp_desc = ForExp (S.symbol $2,
                                      global_escape,
                                      { exp_desc = IntExp 0; exp_pos = $startpos },
                                      { exp_desc = IntExp 0; exp_pos = $startpos },
                                      { exp_desc = IntExp 0; exp_pos = $startpos });
                   exp_pos = $startpos } }
| BREAK { { exp_desc = BreakExp; exp_pos = $startpos } }
| LET decs IN expseq END { { exp_desc = LetExp ($2, $4); exp_pos = $startpos } }

assign:
| lvalue ASSIGN exp { { exp_desc = AssignExp ($1, $3);
                        exp_pos = $startpos($2) } }
| lvalue error { eq_where_assign $startpos $endpos;
                 { exp_desc = AssignExp ($1, { exp_desc = IntExp 0; exp_pos = $startpos } );
                   exp_pos = $startpos } }

arrayDef:
| ID LBRACK exp RBRACK OF exp { { exp_desc = ArrayExp (S.symbol $1, $3, $6);
                                  exp_pos = $startpos } }

recDef:
| ID LBRACE recfields RBRACE { { exp_desc = RecordExp ($3, S.symbol $1);
                                 exp_pos = $startpos } }
| ID LBRACE error { badly_formed_record_definition $startpos $endpos;
                    { exp_desc = RecordExp ([], S.symbol $1);
                      exp_pos = $startpos } }

arithExp:
| MINUS exp %prec UMINUS { { exp_desc = OpExp ({ exp_desc = IntExp 0; exp_pos = $startpos }, MinusOp, $2);
                             exp_pos = $startpos } }
| exp PLUS exp { { exp_desc = OpExp ($1, PlusOp, $3);
                   exp_pos = $startpos($2) } }
| exp MINUS exp { { exp_desc = OpExp ($1, MinusOp, $3);
                    exp_pos = $startpos($2) } }
| exp TIMES exp { { exp_desc = OpExp ($1, TimesOp, $3);
                    exp_pos = $startpos($2) } }
| exp DIV exp { { exp_desc = OpExp ($1, DivideOp, $3);
                  exp_pos = $startpos($2) } }
| exp GT exp { { exp_desc = OpExp ($1, GtOp, $3);
                 exp_pos = $startpos($2) } }
| exp LT exp { { exp_desc = OpExp ($1, LtOp, $3);
                 exp_pos = $startpos($2) } }
| exp GTEQ exp { { exp_desc = OpExp ($1, GeOp, $3);
                   exp_pos = $startpos($2) } }
| exp LTEQ exp { { exp_desc = OpExp ($1, LeOp, $3);
                   exp_pos = $startpos($2) } }
| exp EQ exp { { exp_desc = OpExp ($1, EqOp, $3);
                 exp_pos = $startpos($2) } }
| exp NEQ exp { { exp_desc = OpExp ($1, NeqOp, $3);
                exp_pos = $startpos($2) } }
| exp OR exp { { exp_desc = OpExp ($1, OrOp, $3);
                 exp_pos = $startpos($2) } }
| exp AND exp { { exp_desc = OpExp ($1, AndOp, $3);
                  exp_pos = $startpos($2) } }

funcall:
| ID LPAREN explist RPAREN { { exp_desc = CallExp (S.symbol $1, $3);
                               exp_pos = $startpos } }

explist:
| { [] }
| exp explistMore { $1 :: $2 }

explistMore:
| { [] }
| COMMA exp explistMore { $2 :: $3 }

lvalue:
| subscriptLvalue { $1 }
| dotLvalue { $1 }
| ID { { var_desc = SimpleVar (S.symbol $1);
         var_pos = $startpos } }

dotLvalue:
| ID DOT ID { { var_desc =
                  FieldVar (
                    { var_desc = SimpleVar (S.symbol $1);
                      var_pos = $startpos },
                    S.symbol $3);
                var_pos = $startpos($3) } }
| subscriptLvalue DOT ID { { var_desc = FieldVar ($1, S.symbol $3);
                           var_pos = $startpos($3) } }
| dotLvalue DOT ID { { var_desc = FieldVar ($1, S.symbol $3);
                       var_pos = $startpos($3) } }

subscriptLvalue:
| ID LBRACK exp RBRACK { { var_desc =
                             SubscriptVar (
                               { var_desc = SimpleVar (S.symbol $1);
                                 var_pos = $startpos },
                               $3);
                           var_pos = $startpos($3) } }
| subscriptLvalue LBRACK exp RBRACK { { var_desc = SubscriptVar ($1, $3);
                                        var_pos = $startpos($3) } }
| dotLvalue LBRACK exp RBRACK { { var_desc = SubscriptVar ($1, $3);
                                  var_pos = $startpos($3); } }

recfields:
| { [] }
| ID EQ exp recfieldsMore { ( { reclabel_name = S.symbol $1;
                                reclabel_pos = $startpos },
                              $3 ) :: $4 }

recfieldsMore:
| { [] }
| COMMA ID EQ exp recfieldsMore { ( { reclabel_name = S.symbol $2;
                                      reclabel_pos = $startpos($2) },
                                    $4 ) :: $5 }
