%{
open Ast
%}

/* Tokens */
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT LE GT GE AND OR BANG
%token ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
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

/* Priorités */
/* *** CHANGEMENT: LPAR et LBRACKET doivent avoir la priorité la plus forte *** */
%left LPAR LBRACKET

%right UMINUS UNOT UDEREF UADDR
%left STAR SLASH PERCENT
%left PLUS MINUS
%nonassoc EQ NEQ LT LE GT GE
%left AND
%left OR
%right ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%nonassoc CAST

%start file
%type<Ast.decl list> file

%%

file:
  | decl_list EOF { $1 }

/* *** CHANGEMENT: On sépare clairement fonction et variable *** */
decl_list:
  | /* vide */ { [] }
  | decl decl_list { $1 :: $2 }

decl:
  | vardecl { $1 }
  | fundef  { $1 }


/* Déclaration de variables */
vardecl:
  | type_spec id_list SEMI         { VarDecl($1, $2) }

/* Définition de fonction */
fundef:
  | type_spec ID LPAR param_list_opt RPAR block
      { FunDef($1, $2, $4, $6) }

id_list:
  | ID { [$1] }
  | ID COMMA id_list { $1 :: $3 }

/* Types */
type_spec:
  | base_type star_list 
    { List.fold_left (fun acc _ -> Pointer acc) $1 $2 }

star_list:
  | /* vide */ { [] }
  | STAR star_list { () :: $2 }

base_type:
  | type_attr_list base_type_keyword 
    { $2 }

type_attr_list:
  | /* vide */ { [] }
  | type_attr type_attr_list { $1 :: $2 }

type_attr:
  | SIGNED { Signed }
  | UNSIGNED { Unsigned }
  | SHORT { Short }
  | LONG { Long }

base_type_keyword:
  | VOID { Void }
  | CHAR { Char }
  | INT { Int }
  | FLOAT { Float }
  | DOUBLE { Double }

/* Paramètres de fonction */
param_list_opt:
  | /* vide */ { [] }
  | param_list { $1 }

param_list:
  | type_spec ID { [($1, $2)] }
  | type_spec ID COMMA param_list { ($1, $2) :: $4 }

/* Bloc */
block:
  | LBRACE var_decl_list instr_list RBRACE 
      { Block($2, $3) }

var_decl_list:
  | /* vide */ { [] }
  | var_decl var_decl_list { $1 :: $2 }

var_decl:
  | type_spec id_list SEMI 
      { VarDecl($1, $2) }

/* Instructions */
instr_list:
  | /* vide */ { [] }
  | instr instr_list { $1 :: $2 }

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
  | FOR LPAR expr_opt SEMI expr_opt SEMI expr_opt RPAR instr
      { For($3, $5, $7, $9) }

expr_opt:
  | /* vide */ { None }
  | expr { Some $1 }

/* Expressions */
expr:
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
  
  /* Appel de fonction */
  | ID LPAR arg_list RPAR
      { Call($1, $3) }
  
  /* Accès tableau */
  | expr LBRACKET expr RBRACKET
      { ArrayAccess($1, $3) }
  
  /* sizeof */
  | SIZEOF LPAR type_spec RPAR
      { SizeOf($3) }
  
  /* Cast */
  | LPAR type_spec RPAR expr %prec CAST
      { Cast($2, $4) }
  
  /* Opérateurs unaires */
  | MINUS expr %prec UMINUS
      { UnOp("-", $2) }
  | BANG expr %prec UNOT
      { UnOp("!", $2) }
  | STAR expr %prec UDEREF
      { UnOp("*", $2) }
  | AMPERSAND expr %prec UADDR
      { UnOp("&", $2) }
  
  /* Opérateurs binaires */
  | expr PLUS expr
      { BinOp("+", $1, $3) }
  | expr MINUS expr
      { BinOp("-", $1, $3) }
  | expr STAR expr
      { BinOp("*", $1, $3) }
  | expr SLASH expr
      { BinOp("/", $1, $3) }
  | expr PERCENT expr
      { BinOp("%", $1, $3) }
  
  | expr LT expr
      { BinOp("<", $1, $3) }
  | expr GT expr
      { BinOp(">", $1, $3) }
  | expr LE expr
      { BinOp("<=", $1, $3) }
  | expr GE expr
      { BinOp(">=", $1, $3) }
  | expr EQ expr
      { BinOp("==", $1, $3) }
  | expr NEQ expr
      { BinOp("!=", $1, $3) }
  
  | expr AND expr
      { BinOp("&&", $1, $3) }
  | expr OR expr
      { BinOp("||", $1, $3) }
  
  /* Affectations */
  | expr ASSIGN expr
      { BinOp("=", $1, $3) }
  | expr PLUSEQ expr
      { BinOp("+=", $1, $3) }
  | expr MINUSEQ expr
      { BinOp("-=", $1, $3) }
  | expr STAREQ expr
      { BinOp("*=", $1, $3) }
  | expr SLASHEQ expr
      { BinOp("/=", $1, $3) }
  | expr PERCENTEQ expr
      { BinOp("%=", $1, $3) }

arg_list:
  | /* vide */ { [] }
  | expr { [$1] }
  | expr COMMA arg_list { $1 :: $3 }
