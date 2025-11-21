%{
open Ast

let make_for init cond step body =
  match init with
  | `Expr e -> For(Some e, cond, step, body)
  | `Decl d -> Block([d], [For(None, cond, step, body)])
  | `None -> For(None, cond, step, body)

%}

/* Tokens */
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT LE GT GE AND OR BANG
%token ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%token INC DEC  /* ++ et -- */
%token LPAR RPAR LBRACKET RBRACKET LBRACE RBRACE SEMI COMMA
%token RETURN IF ELSE WHILE DO FOR SIZEOF
%token VOID CHAR INT FLOAT DOUBLE
%token SIGNED UNSIGNED SHORT LONG
%token AMPERSAND
%token<string> ID
%token<int> INTCONST
%token<float> FLOATCONST
%token<string> STRINGCONST
%token EOF

/* Priorités et associativités */
%right ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%right UMINUS UNOT UDEREF UADDR
%left LPAR LBRACKET
%nonassoc CAST
%nonassoc ELSE

%start file
%type<Ast.decl list> file

%%

/* Point d'entrée */
file:
  | decl_list EOF { $1 }
;

decl_list:
  | /* vide */ { [] }
  | decl decl_list { $1 :: $2 }
;

/* Déclarations globales */
decl:
  | vardecl { $1 }
  | fundef  { $1 }
;

/* Déclarations de variables globales */
vardecl:
  | type_spec declarator_list SEMI
      { VarDecl($1, $2) }
  | type_spec ID ASSIGN expr SEMI
      { VarDeclInit($1, $2, $4) }
;

/* Liste de déclarateurs (pour int x, y, z;) */
declarator_list:
  | ID                       { [$1] }
  | ID COMMA declarator_list { $1 :: $3 }
;

/* Définition de fonction */
fundef:
  | type_spec ID LPAR param_list_opt RPAR block
      { FunDef($1, $2, $4, $6) }
;

/* Spécificateur de type complet */
type_spec:
  | base_type pointer_depth 
      { List.fold_left (fun acc _ -> Pointer acc) $1 $2 }
;

/* Niveau de pointeur (*, **, ***, etc.) */
pointer_depth:
  | /* vide */           { [] }
  | STAR pointer_depth   { () :: $2 }
;

/* Type de base avec attributs optionnels */
base_type:
  | type_attr_list base_type_keyword 
      { $2 }
;

type_attr_list:
  | /* vide */                   { [] }
  | type_attr type_attr_list     { $1 :: $2 }
;

type_attr:
  | SIGNED   { Signed }
  | UNSIGNED { Unsigned }
  | SHORT    { Short }
  | LONG     { Long }
;

base_type_keyword:
  | VOID   { Void }
  | CHAR   { Char }
  | INT    { Int }
  | FLOAT  { Float }
  | DOUBLE { Double }
;

/* Paramètres de fonction */
param_list_opt:
  | /* vide */ { [] }
  | param_list { $1 }
;

param_list:
  | param                    { [$1] }
  | param COMMA param_list   { $1 :: $3 }
;

param:
  | type_spec ID { ($1, $2) }
;

/* Bloc d'instructions */
block:
  | LBRACE var_decl_list instr_list RBRACE 
      { Block($2, $3) }
;

/* Déclarations de variables locales */
var_decl_list:
  | /* vide */                 { [] }
  | var_decl var_decl_list     { $1 :: $2 }
;

var_decl:
  | type_spec declarator_list SEMI
      { VarDecl($1, $2) }
  | type_spec ID ASSIGN expr SEMI
      { VarDeclInit($1, $2, $4) }
;

/* Liste d'instructions */
instr_list:
  | /* vide */             { [] }
  | instr instr_list       { $1 :: $2 }
;

/* Instructions */
instr:
  | expr SEMI 
      { Expr($1) }
  | SEMI 
      { Empty }
  | block 
      { $1 }
  | RETURN expr SEMI 
      { Return($2) }
  | IF LPAR expr RPAR instr ELSE instr
      { If($3, $5, Some $7) }
  | IF LPAR expr RPAR instr %prec ELSE
      { If($3, $5, None) }
  | WHILE LPAR expr RPAR instr
      { While($3, $5) }
  | DO instr WHILE LPAR expr RPAR SEMI
      { DoWhile($2, $5) }
  | FOR LPAR for_init SEMI expr_opt SEMI expr_opt RPAR instr
    { make_for $3 $5 $7 $9 }
;

expr_opt:
  | /* vide */ { None }
  | expr       { Some $1 }
;

/* Initialisation du for : peut être une expression, une déclaration sans SEMI, ou vide */
for_init:
  | /* vide */ 
      { `None }
  | type_spec ID ASSIGN expr
      { `Decl (VarDeclInit($1, $2, $4)) }
  | type_spec declarator_list
      { `Decl (VarDecl($1, $2)) }
  | for_init_expr 
      { `Expr $1 }
;

/* Expression dans l'initialisation du for (priorité plus basse pour éviter conflits) */
for_init_expr:
  | assign_expr { $1 }
;

/* Expression d'affectation (et expressions de priorité inférieure) */
assign_expr:
  | or_expr 
      { $1 }
  | or_expr ASSIGN assign_expr
      { BinOp("=", $1, $3) }
  | or_expr PLUSEQ assign_expr
      { BinOp("+=", $1, $3) }
  | or_expr MINUSEQ assign_expr
      { BinOp("-=", $1, $3) }
  | or_expr STAREQ assign_expr
      { BinOp("*=", $1, $3) }
  | or_expr SLASHEQ assign_expr
      { BinOp("/=", $1, $3) }
  | or_expr PERCENTEQ assign_expr
      { BinOp("%=", $1, $3) }
;

or_expr:
  | and_expr { $1 }
  | or_expr OR and_expr
      { BinOp("||", $1, $3) }
;

and_expr:
  | eq_expr { $1 }
  | and_expr AND eq_expr
      { BinOp("&&", $1, $3) }
;

eq_expr:
  | rel_expr { $1 }
  | eq_expr EQ rel_expr
      { BinOp("==", $1, $3) }
  | eq_expr NEQ rel_expr
      { BinOp("!=", $1, $3) }
;

rel_expr:
  | add_expr { $1 }
  | rel_expr LT add_expr
      { BinOp("<", $1, $3) }
  | rel_expr GT add_expr
      { BinOp(">", $1, $3) }
  | rel_expr LE add_expr
      { BinOp("<=", $1, $3) }
  | rel_expr GE add_expr
      { BinOp(">=", $1, $3) }
;

add_expr:
  | mult_expr { $1 }
  | add_expr PLUS mult_expr
      { BinOp("+", $1, $3) }
  | add_expr MINUS mult_expr
      { BinOp("-", $1, $3) }
;

mult_expr:
  | unary_expr { $1 }
  | mult_expr STAR unary_expr
      { BinOp("*", $1, $3) }
  | mult_expr SLASH unary_expr
      { BinOp("/", $1, $3) }
  | mult_expr PERCENT unary_expr
      { BinOp("%", $1, $3) }
;

unary_expr:
  | postfix_expr 
      { $1 }
  | INC unary_expr
      { UnOp("++", $2) }
  | DEC unary_expr
      { UnOp("--", $2) }
  | MINUS unary_expr
      { UnOp("-", $2) }
  | BANG unary_expr
      { UnOp("!", $2) }
  | STAR unary_expr
      { UnOp("*", $2) }
  | AMPERSAND unary_expr
      { UnOp("&", $2) }
  | SIZEOF LPAR type_spec RPAR
      { SizeOf($3) }
  | LPAR type_spec RPAR unary_expr
      { Cast($2, $4) }
;

postfix_expr:
  | primary_expr 
      { $1 }
  | postfix_expr LBRACKET expr RBRACKET
      { ArrayAccess($1, $3) }
  | ID LPAR arg_list RPAR
      { Call($1, $3) }
  | postfix_expr INC
      { UnOp("post++", $1) }
  | postfix_expr DEC
      { UnOp("post--", $1) }
;

primary_expr:
  | INTCONST 
      { Const(IntConst $1) }
  | FLOATCONST 
      { Const(FloatConst $1) }
  | STRINGCONST 
      { Const(StringConst $1) }
  | ID 
      { Id $1 }
  | LPAR expr RPAR 
      { Parens $2 }
;

/* Expressions - point d'entrée général */
expr:
  | assign_expr { $1 }
;

/* Arguments d'appel de fonction */
arg_list:
  | /* vide */           { [] }
  | non_empty_arg_list   { $1 }
;

non_empty_arg_list:
  | assign_expr                             { [$1] }
  | assign_expr COMMA non_empty_arg_list    { $1 :: $3 }
;