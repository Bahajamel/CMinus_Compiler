
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
  | VarDeclInit of ctype * string * expr
  | FunDef of ctype * string * (ctype * string) list * instr

(* Fonctions utilitaires pour la conversion en chaînes *)

let rec string_of_type = function
  | Void -> "void"
  | Char -> "char"
  | Int -> "int"
  | Float -> "float"
  | Double -> "double"
  | Pointer t -> (string_of_type t) ^ "*"
  | Signed -> "signed int"
  | Unsigned -> "unsigned int"
  | Short -> "short int"
  | Long -> "long int"

let string_of_const = function
  | IntConst i -> string_of_int i
  | FloatConst f -> string_of_float f
  | StringConst s -> "\"" ^ String.escaped s ^ "\""

let rec string_of_expr = function
  | Id s -> s
  | Const c -> string_of_const c
  | BinOp(op, e1, e2) ->
      "(" ^ (string_of_expr e1) ^ " " ^ op ^ " " ^ (string_of_expr e2) ^ ")"
  | UnOp(op, e) ->
      op ^ "(" ^ (string_of_expr e) ^ ")"
  | Call(f, args) ->
      f ^ "(" ^ (String.concat ", " (List.map string_of_expr args)) ^ ")"
  | ArrayAccess(arr, idx) ->
      (string_of_expr arr) ^ "[" ^ (string_of_expr idx) ^ "]"
  | SizeOf t ->
      "sizeof(" ^ (string_of_type t) ^ ")"
  | Cast(t, e) ->
      "(" ^ (string_of_type t) ^ ")(" ^ (string_of_expr e) ^ ")"
  | Parens e ->
      "(" ^ (string_of_expr e) ^ ")"

(* Fonctions d'affichage pour le débogage avec indentation *)

let rec print_type t =
  print_string (string_of_type t)

let print_const c =
  print_string (string_of_const c)

let rec print_expr e =
  match e with
  | Id s -> Printf.printf "Id(%s)" s
  | Const c -> 
      print_string "Const("; 
      print_const c; 
      print_string ")"

  (* Ajout des opérateurs postfixés *)
  | UnOp("post++", e) ->
      print_string "PostInc("; print_expr e; print_string ")"
  | UnOp("post--", e) ->
      print_string "PostDec("; print_expr e; print_string ")"

  (* Tous les autres unaires (préfixes) *)
  | UnOp(op, e) ->
      Printf.printf "UnOp(%s, " op;
      print_expr e;
      print_string ")"

  | BinOp(op, e1, e2) ->
      Printf.printf "BinOp(%s, " op;
      print_expr e1;
      print_string ", ";
      print_expr e2;
      print_string ")"

  | Call(f, args) ->
      Printf.printf "Call(%s, [" f;
      List.iteri (fun i e -> 
        if i > 0 then print_string ", ";
        print_expr e
      ) args;
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


let rec print_instr indent instr =
  let print_indent () = print_string (String.make (indent * 2) ' ') in
  print_indent ();
  match instr with
  | Expr e ->
      print_string "Expr(";
      print_expr e;
      print_string ");\n"
  | Empty ->
      print_string "Empty;\n"
  | Block(decls, instrs) ->
      print_string "Block {\n";
      List.iter (print_decl (indent + 1)) decls;
      List.iter (print_instr (indent + 1)) instrs;
      print_indent ();
      print_string "}\n"
  | Return e ->
      print_string "Return(";
      print_expr e;
      print_string ");\n"
  | If(cond, then_i, else_opt) ->
      print_string "If (";
      print_expr cond;
      print_string ") {\n";
      print_instr (indent + 1) then_i;
      (match else_opt with
       | Some else_i ->
           print_indent ();
           print_string "} Else {\n";
           print_instr (indent + 1) else_i;
           print_indent ();
           print_string "}\n"
       | None ->
           print_indent ();
           print_string "}\n")
  | While(cond, body) ->
      print_string "While (";
      print_expr cond;
      print_string ") {\n";
      print_instr (indent + 1) body;
      print_indent ();
      print_string "}\n"
  | DoWhile(body, cond) ->
      print_string "Do {\n";
      print_instr (indent + 1) body;
      print_indent ();
      print_string "} While (";
      print_expr cond;
      print_string ");\n"
  | For(init, cond, step, body) ->
      print_string "For (";
      (match init with Some e -> print_expr e | None -> ());
      print_string "; ";
      (match cond with Some e -> print_expr e | None -> ());
      print_string "; ";
      (match step with Some e -> print_expr e | None -> ());
      print_string ") {\n";
      print_instr (indent + 1) body;
      print_indent ();
      print_string "}\n"

and print_decl indent = function
  | VarDecl(t, ids) ->
      let print_indent () = print_string (String.make (indent * 2) ' ') in
      print_indent ();
      print_string "VarDecl(";
      print_type t;
      print_string ", [";
      List.iteri (fun i id -> 
        if i > 0 then print_string ", ";
        print_string id
      ) ids;
      print_string "]);\n"
  | VarDeclInit(t, id, expr) ->
      let print_indent () = print_string (String.make (indent * 2) ' ') in
      print_indent ();
      print_string "VarDeclInit(";
      print_type t;
      Printf.printf ", %s = " id;
      print_expr expr;
      print_string ");\n"
  | FunDef(ret_type, name, params, body) ->
      let print_indent () = print_string (String.make (indent * 2) ' ') in
      print_indent ();
      Printf.printf "FunDef(%s, " name;
      print_type ret_type;
      print_string ", [";
      List.iteri (fun i (t, id) ->
        if i > 0 then print_string ", ";
        print_type t;
        Printf.printf " %s" id
      ) params;
      print_string "]) {\n";
      print_instr (indent + 1) body;
      print_indent ();
      print_string "}\n"

(* Fonction d'affichage du programme complet *)
let print_program decls =
  print_endline "=== Programme ===";
  List.iter (print_decl 0) decls;
  print_endline "================"

