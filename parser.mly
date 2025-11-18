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

/* Priorités et associativités */
%right ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%right UMINUS UNOT UDEREF UADDR
%nonassoc CAST
%left LBRACKET
%left LPAR

%start<Ast.decl list> file

%%

file:
  | dl = list(decl_or_fundef) EOF { dl }

decl_or_fundef:
  | t = type_spec ids = separated_nonempty_list(COMMA, ID) SEMI 
    { VarDecl(t, ids) }
  | t = type_spec name = ID LPAR params = param_list_opt RPAR body = block
    { FunDef(t, name, params, body) }

/* Types */
type_spec:
  | t = base_type ptrs = list(STAR) 
    { List.fold_left (fun acc _ -> Pointer acc) t ptrs }

base_type:
  | attrs = list(type_attr) bt = base_type_keyword 
    { 
      (* On applique les attributs sur le type de base *)
      (* Note: dans votre AST, Signed/Unsigned/Short/Long sont des types,
         donc on les traite comme tels *)
      List.fold_left (fun acc attr -> attr) bt attrs 
    }

type_attr:
  | SIGNED   { Signed }
  | UNSIGNED { Unsigned }
  | SHORT    { Short }
  | LONG     { Long }

base_type_keyword:
  | VOID   { Void }
  | CHAR   { Char }
  | INT    { Int }
  | FLOAT  { Float }
  | DOUBLE { Double }

/* Paramètres de fonction */
param_list_opt:
  | /* vide */ { [] }
  | pl = separated_nonempty_list(COMMA, param) { pl }

param:
  | t = type_spec name = ID { (t, name) }

/* Bloc */
block:
  | LBRACE decls = list(var_decl) instrs = list(instr) RBRACE 
    { Block(decls, instrs) }

var_decl:
  | t = type_spec ids = separated_nonempty_list(COMMA, ID) SEMI 
    { VarDecl(t, ids) }

/* Instructions */
instr:
  | e = expr SEMI 
    { Expr(e) }
  | SEMI 
    { Empty }
  | b = block 
    { b }
  | RETURN e = expr SEMI 
    { Return(e) }
  | IF LPAR cond = expr RPAR then_i = instr ELSE else_i = instr
    { If(cond, then_i, Some else_i) }
  | IF LPAR cond = expr RPAR then_i = instr %prec ELSE
    { If(cond, then_i, None) }
  | WHILE LPAR cond = expr RPAR body = instr
    { While(cond, body) }
  | DO body = instr WHILE LPAR cond = expr RPAR SEMI
    { DoWhile(body, cond) }
  | FOR LPAR init = expr_opt SEMI cond = expr_opt SEMI step = expr_opt RPAR body = instr
    { For(init, cond, step, body) }

expr_opt:
  | /* vide */ { None }
  | e = expr   { Some e }

/* Expressions */
expr:
  /* Constantes et identifiants */
  | i = INTCONST 
    { Const(IntConst i) }
  | f = FLOATCONST 
    { Const(FloatConst f) }
  | s = STRINGCONST 
    { Const(StringConst s) }
  | id = ID 
    { Id id }
  
  /* Parenthèses */
  | LPAR e = expr RPAR 
    { Parens e }
  
  /* Appel de fonction */
  | f = ID LPAR args = separated_list(COMMA, expr) RPAR
    { Call(f, args) }
  
  /* Accès tableau */
  | arr = expr LBRACKET idx = expr RBRACKET
    { ArrayAccess(arr, idx) }
  
  /* sizeof */
  | SIZEOF LPAR t = type_spec RPAR
    { SizeOf(t) }
  
  /* Cast */
  | LPAR t = type_spec RPAR e = expr %prec CAST
    { Cast(t, e) }
  
  /* Opérateurs unaires */
  | MINUS e = expr %prec UMINUS
    { UnOp("-", e) }
  | BANG e = expr %prec UNOT
    { UnOp("!", e) }
  | STAR e = expr %prec UDEREF
    { UnOp("*", e) }
  | AMPERSAND e = expr %prec UADDR
    { UnOp("&", e) }
  
  /* Opérateurs arithmétiques */
  | e1 = expr PLUS e2 = expr
    { BinOp("+", e1, e2) }
  | e1 = expr MINUS e2 = expr
    { BinOp("-", e1, e2) }
  | e1 = expr STAR e2 = expr
    { BinOp("*", e1, e2) }
  | e1 = expr SLASH e2 = expr
    { BinOp("/", e1, e2) }
  | e1 = expr PERCENT e2 = expr
    { BinOp("%", e1, e2) }
  
  /* Opérateurs de comparaison */
  | e1 = expr LT e2 = expr
    { BinOp("<", e1, e2) }
  | e1 = expr GT e2 = expr
    { BinOp(">", e1, e2) }
  | e1 = expr LE e2 = expr
    { BinOp("<=", e1, e2) }
  | e1 = expr GE e2 = expr
    { BinOp(">=", e1, e2) }
  | e1 = expr EQ e2 = expr
    { BinOp("==", e1, e2) }
  | e1 = expr NEQ e2 = expr
    { BinOp("!=", e1, e2) }
  
  /* Opérateurs logiques */
  | e1 = expr AND e2 = expr
    { BinOp("&&", e1, e2) }
  | e1 = expr OR e2 = expr
    { BinOp("||", e1, e2) }
  
  /* Opérateurs d'affectation */
  | e1 = expr ASSIGN e2 = expr
    { BinOp("=", e1, e2) }
  | e1 = expr PLUSEQ e2 = expr
    { BinOp("+=", e1, e2) }
  | e1 = expr MINUSEQ e2 = expr
    { BinOp("-=", e1, e2) }
  | e1 = expr STAREQ e2 = expr
    { BinOp("*=", e1, e2) }
  | e1 = expr SLASHEQ e2 = expr
    { BinOp("/=", e1, e2) }
  | e1 = expr PERCENTEQ e2 = expr
    { BinOp("%=", e1, e2) }