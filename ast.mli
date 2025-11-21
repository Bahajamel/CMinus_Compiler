(* Interface pour le module Ast *)

(* Types de base du langage C *)
type ctype =
  | Void
  | Char
  | Int
  | Float
  | Double
  | Pointer of ctype
  | Signed
  | Unsigned
  | Short
  | Long

(* Constantes *)
type constant =
  | IntConst of int
  | FloatConst of float
  | StringConst of string

(* Expressions *)
type expr =
  | Id of string
  | Const of constant
  | BinOp of string * expr * expr
  | UnOp of string * expr
  | Call of string * expr list
  | ArrayAccess of expr * expr
  | SizeOf of ctype
  | Cast of ctype * expr
  | Parens of expr

(* Instructions *)
type instr =
  | Expr of expr
  | Empty
  | Block of decl list * instr list
  | Return of expr
  | If of expr * instr * instr option
  | While of expr * instr
  | DoWhile of instr * expr
  | For of expr option * expr option * expr option * instr

(* Déclarations *)
and decl =
  | VarDecl of ctype * string list
  | VarDeclInit of ctype * string * expr
  | FunDef of ctype * string * (ctype * string) list * instr

(* Fonctions de conversion en chaînes *)
val string_of_type : ctype -> string
val string_of_const : constant -> string
val string_of_expr : expr -> string

(* Fonctions d'affichage pour le débogage *)
val print_type : ctype -> unit
val print_const : constant -> unit
val print_expr : expr -> unit
val print_instr : int -> instr -> unit
val print_decl : int -> decl -> unit
val print_program : decl list -> unit