(* Implémentation de l'interface ast.mli *)

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

(* Fonctions d'affichage améliorées *)

let rec print_type = function
  | Void -> print_string "void"
  | Char -> print_string "char"
  | Int -> print_string "int"
  | Float -> print_string "float"
  | Double -> print_string "double"
  | Pointer t -> print_type t; print_string "*"
  | Signed -> print_string "signed"
  | Unsigned -> print_string "unsigned"
  | Short -> print_string "short"
  | Long -> print_string "long"

let print_const = function
  | IntConst i -> Printf.printf "%d" i
  | FloatConst f -> Printf.printf "%f" f
  | StringConst s -> Printf.printf "\"%s\"" s

let rec print_expr = function
  | Id s -> Printf.printf "Id(%s)" s
  | Const c -> print_string "Const("; print_const c; print_string ")"
  | BinOp(op, e1, e2) ->
      Printf.printf "BinOp(%s, " op;
      print_expr e1;
      print_string ", ";
      print_expr e2;
      print_string ")"
  | UnOp(op, e) ->
      Printf.printf "UnOp(%s, " op;
      print_expr e;
      print_string ")"
  | Call(f, args) ->
      Printf.printf "Call(%s, [" f;
      List.iter (fun e -> print_expr e; print_string "; ") args;
      print_string "])"
  | ArrayAccess(arr, idx) ->
      print_string "ArrayAccess(";
      print_expr arr;
      print_string ", ";
      print_expr idx;
      print_string ")"
  | SizeOf t ->
      print_string "SizeOf(";
      print_type t;
      print_string ")"
  | Cast(t, e) ->
      print_string "Cast(";
      print_type t;
      print_string ", ";
      print_expr e;
      print_string ")"
  | Parens e ->
      print_string "Parens(";
      print_expr e;
      print_string ")"

let rec print_instr = function
  | Expr e ->
      print_string "Expr(";
      print_expr e;
      print_string ")\n"
  | Empty ->
      print_string "Empty\n"
  | Block(decls, instrs) ->
      print_string "Block {\n";
      List.iter print_decl decls;
      List.iter print_instr instrs;
      print_string "}\n"
  | Return e ->
      print_string "Return(";
      print_expr e;
      print_string ")\n"
  | If(cond, then_i, else_opt) ->
      print_string "If(";
      print_expr cond;
      print_string ")\n";
      print_instr then_i;
      (match else_opt with
       | Some else_i ->
           print_string "Else\n";
           print_instr else_i
       | None -> ())
  | While(cond, body) ->
      print_string "While(";
      print_expr cond;
      print_string ")\n";
      print_instr body
  | DoWhile(body, cond) ->
      print_string "DoWhile\n";
      print_instr body;
      print_string "While(";
      print_expr cond;
      print_string ")\n"
  | For(init, cond, step, body) ->
      print_string "For(";
      (match init with Some e -> print_expr e | None -> ());
      print_string "; ";
      (match cond with Some e -> print_expr e | None -> ());
      print_string "; ";
      (match step with Some e -> print_expr e | None -> ());
      print_string ")\n";
      print_instr body

and print_decl = function
  | VarDecl(t, ids) ->
      print_string "VarDecl(";
      print_type t;
      print_string ", [";
      List.iter (fun id -> Printf.printf "%s; " id) ids;
      print_string "])\n"
  | FunDef(ret_type, name, params, body) ->
      Printf.printf "FunDef(%s, " name;
      print_type ret_type;
      print_string ", [";
      List.iter (fun (t, id) ->
        print_type t;
        Printf.printf " %s; " id
      ) params;
      print_string "])\n";
      print_instr body