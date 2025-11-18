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

type constant =
  | IntConst of int
  | FloatConst of float
  | StringConst of string

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

type instr =
  | Expr of expr
  | Empty
  | Block of decl list * instr list
  | Return of expr
  | If of expr * instr * instr option
  | While of expr * instr
  | DoWhile of instr * expr
  | For of expr option * expr option * expr option * instr

and decl =
  | VarDecl of ctype * string list
  | FunDef of ctype * string * (ctype * string) list * instr

val print_expr : expr -> unit
val print_instr : instr -> unit
val print_decl : decl -> unit
