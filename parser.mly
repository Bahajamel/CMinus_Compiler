%{
open Ast

let make_for init cond step body =
  match init with
  | `Expr e -> For(Some e, cond, step, body)
  | `Decl d -> Block([d], [For(None, cond, step, body)])
  | `None -> For(None, cond, step, body)

(* Helpers to build ctype from attributes + base type *)

let has_attr a attrs =
  List.exists (fun x -> x = a) attrs

(* Normalize integer type: choose one of Int / Unsigned / Short / Long *)
let normalize_int_type attrs =
  if has_attr Unsigned attrs then Unsigned
  else if has_attr Long attrs then Long
  else if has_attr Short attrs then Short
  else (* default / signed *) Int

(* For float/double we simply ignore modifiers here.
   Note: C doesn't define "long float"; only "long double".
   With this AST we can't represent "long float" distinctly. *)
let normalize_float_type attrs base =
  match base with
  | Float -> Float
  | Double -> Double
  | _ -> base
(* Combine attributs + type de base *)
let make_ctype attrs base =
  match base with
  | Void -> Void
  | Char -> Char
  | Int -> normalize_int_type attrs
  | Float | Double as b -> normalize_float_type attrs b
  | Signed | Unsigned | Short | Long ->
      (* Should not happen, base is always from base_type_keyword *)
      Int

let rec add_pointers t depth =
  if depth <= 0 then t else add_pointers (Pointer t) (depth - 1)
%}
(* -------------------------- TOKENS et PRIORITÉS ----------------------- *)
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

/* ---------------------------------------------
   Fichier
----------------------------------------------*/
file:
  | decl_list EOF { $1 }
;

decl_list:
  | /* vide */          { [] }
  | decl decl_list      { $1 :: $2 }
;

/* ---------------------------------------------
   Déclarations globales
----------------------------------------------*/
decl:
  | vardecl             { $1 }
  | fundef              { $1 }
;

vardecl:
  | type_spec declarator_list SEMI
      { VarDecl($1, $2) }
  | type_spec ID ASSIGN expr SEMI
      { VarDeclInit($1, $2, $4) }
;

declarator_list:
  | ID                       { [$1] }
  | ID COMMA declarator_list { $1 :: $3 }
;

/* ---------------------------------------------
   Fonctions
----------------------------------------------*/
fundef:
  | type_spec ID LPAR param_list_opt RPAR block
      { FunDef($1, $2, $4, $6) }
;

/* ---------- Types ---------- */

/* full type = attributes + base + pointer depth */
type_spec:
  | type_attr_list base_type_keyword pointer_depth
      { add_pointers (make_ctype $1 $2) $3 }
;

/* Modifiers list (signed/unsigned/short/long) */
type_attr_list:
  | /* vide */                 { [] }
  | type_attr type_attr_list   { $1 :: $2 }
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

/* Pointer depth: count '*' */
pointer_depth:
  | /* vide */           { 0 }
  | STAR pointer_depth   { 1 + $2 }
;

/* ---------------------------------------------
   Paramètres de fonction
----------------------------------------------*/
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

/* ---------------------------------------------
   Blocs
----------------------------------------------*/
block:
  | LBRACE var_decl_list instr_list RBRACE 
      { Block($2, $3) }
;

var_decl_list:
  | /* vide */               { [] }
  | var_decl var_decl_list   { $1 :: $2 }
;

var_decl:
  | type_spec declarator_list SEMI
      { VarDecl($1, $2) }
  | type_spec ID ASSIGN expr SEMI
      { VarDeclInit($1, $2, $4) }
;

instr_list:
  | /* vide */         { [] }
  | instr instr_list   { $1 :: $2 }
;

/* ---------------------------------------------
   Instructions (dangling else : matched / unmatched)
----------------------------------------------*/

/* Une instruction est soit "matched" soit "unmatched" */
instr:
  | matched_instr   { $1 }
  | unmatched_instr { $1 }
;

/* matched_instr : tous les if ont un else fixé */
matched_instr:
  | expr SEMI 
      { Expr($1) }
  | SEMI 
      { Empty }
  | block 
      { $1 }
  | RETURN expr SEMI 
      { Return($2) }

  | WHILE LPAR expr RPAR matched_instr
      { While($3, $5) }

  | DO matched_instr WHILE LPAR expr RPAR SEMI
      { DoWhile($2, $5) }

  | FOR LPAR for_init SEMI expr_opt SEMI expr_opt RPAR matched_instr
      { make_for $3 $5 $7 $9 }

  | IF LPAR expr RPAR matched_instr ELSE matched_instr
      { If($3, $5, Some $7) }
;

/* Gestion du dangling-else */
unmatched_instr:
  /* if sans else : then = instr (matched ou unmatched) */
  | IF LPAR expr RPAR instr
      { If($3, $5, None) }

  /* forme canonique du dangling else */
  | IF LPAR expr RPAR matched_instr ELSE unmatched_instr
      { If($3, $5, Some $7) }

  /* boucles dont le corps peut contenir un if pendant */
  | WHILE LPAR expr RPAR unmatched_instr
      { While($3, $5) }

  | DO unmatched_instr WHILE LPAR expr RPAR SEMI
      { DoWhile($2, $5) }

  | FOR LPAR for_init SEMI expr_opt SEMI expr_opt RPAR unmatched_instr
      { make_for $3 $5 $7 $9 }
;

expr_opt:
  | /* vide */ { None }
  | expr       { Some $1 }
;

/* ---------------------------------------------
   For init
----------------------------------------------*/
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

for_init_expr:
  | assign_expr { $1 }
;

/* ---------------------------------------------
   Expressions
----------------------------------------------*/
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
      { UnOp("*", $2) }             /* déréférencement */
  | AMPERSAND unary_expr
      { UnOp("&", $2) }            /* adresse */
  | SIZEOF LPAR type_spec RPAR
      { SizeOf($3) }
  | LPAR type_spec RPAR unary_expr
      { Cast($2, $4) }
;

/* postfix_expr: on met l'appel avant l'identifiant pour éviter les conflits */
postfix_expr:
  | call_expr
      { $1 }
  | primary_expr 
      { $1 }
  | postfix_expr LBRACKET expr RBRACKET
      { ArrayAccess($1, $3) }
  | postfix_expr INC
      { UnOp("post++", $1) }
  | postfix_expr DEC
      { UnOp("post--", $1) }
;

/* Appels */
call_expr:
  | ID LPAR arg_list RPAR
      { Call($1, $3) }
;

/* ---------------------------------------------
   Primaires + concaténation de chaînes
----------------------------------------------*/
primary_expr:
  | INTCONST 
      { Const(IntConst $1) }
  | FLOATCONST 
      { Const(FloatConst $1) }
  | string_literal
      { Const(StringConst $1) }
  | ID 
      { Id $1 }
  | LPAR expr RPAR 
      { Parens $2 }
;

/* "a" "b" → concaténation automatique */
string_literal:
  | STRINGCONST
      { $1 }
  | string_literal STRINGCONST
      { $1 ^ $2 }
;

expr:
  | assign_expr { $1 }
;

/* ---------------------------------------------
   Arguments de fonction
----------------------------------------------*/
arg_list:
  | /* vide */         { [] }
  | non_empty_arg_list { $1 }
;

non_empty_arg_list:
  | expr                          { [$1] }
  | expr COMMA non_empty_arg_list { $1 :: $3 }
;