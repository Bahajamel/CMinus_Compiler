open Ast
module StringMap = Map.Make(String)

exception TypeError of string

(* types simplifiés *)
type simple_type =
  | TInt
  | TFloat
  | TVoid
  | TPointer of simple_type

(* convertir ctype AST -> simple_type *)
let rec to_simple_type = function
  | Int | Char | Short | Long | Signed | Unsigned -> TInt
  | Float | Double -> TFloat
  | Void -> TVoid
  | Pointer t -> TPointer (to_simple_type t)

(* comparaisons / compatibilités *)
let rec simple_eq a b =
  match a, b with
  | TInt, TInt -> true
  | TFloat, TFloat -> true
  | TVoid, TVoid -> true
  | TPointer x, TPointer y -> simple_eq x y
  | _ -> false

(* compatibilité (ex: int -> float acceptable) *)
let compatible expected actual =
  if simple_eq expected actual then true
  else
    match expected, actual with
    | TFloat, TInt -> true   (* promotion int -> float autorisée *)
    | _ -> false

(* résultat arithmétique : si un opérande est float -> float, sinon int *)
let result_arith t1 t2 =
  match t1, t2 with
  | TFloat, _ | _, TFloat -> TFloat
  | TInt, TInt -> TInt
  | _ -> raise (TypeError "Invalid types for arithmetic operation")

(* environnement = variables + fonctions *)
type env = {
  vars : ctype StringMap.t;
  funcs : (ctype * ctype list) StringMap.t;  (* nom -> (ret_type, param_types) *)
}

let empty_env = { vars = StringMap.empty; funcs = StringMap.empty }

let add_var env name typ =
  if StringMap.mem name env.vars then
    raise (TypeError ("Variable "^name^" redeclared"));
  { env with vars = StringMap.add name typ env.vars }

(* Nouvelle fonction : ajouter une variable sans check de redéclaration (pour scope local) *)
let add_var_shadowing env name typ =
  { env with vars = StringMap.add name typ env.vars }

let add_func env name ret params =
  if StringMap.mem name env.funcs then
    raise (TypeError ("Function "^name^" already declared"));
  { env with funcs = StringMap.add name (ret, params) env.funcs }

(* Vérification des expressions *)
let rec check_expr env = function
  | Id x ->
      if not (StringMap.mem x env.vars) then
        raise (TypeError ("Variable "^x^" not declared"));
      to_simple_type (StringMap.find x env.vars)

  | Const c ->
      (match c with
       | IntConst _ -> TInt
       | FloatConst _ -> TFloat
       | StringConst _ -> TPointer TInt)  (* chaîne -> pointeur sur char (char ≡ TInt ici) *)

  | Parens e ->
      check_expr env e

  | BinOp(op, e1, e2) ->
      let t1 = check_expr env e1 in
      let t2 = check_expr env e2 in
      (match op with
       | "+" | "-" | "*" | "/" ->
           (* autoriser int/float mixte *)
           let _ = match t1, t2 with
             | TInt, TInt | TFloat, TFloat | TInt, TFloat | TFloat, TInt -> ()
             | _ -> raise (TypeError ("Arithmetic operator "^op^" type mismatch"))
           in
           result_arith t1 t2

       | "%" ->
           if t1 = TInt && t2 = TInt then TInt
           else raise (TypeError "Modulo operator '%' requires integers")

       | "==" | "!=" | "<" | "<=" | ">" | ">=" ->
           if simple_eq t1 t2 || (t1 = TFloat && t2 = TInt) || (t1 = TInt && t2 = TFloat) then TInt
           else raise (TypeError ("Comparison "^op^" type mismatch"))

       | "&&" | "||" ->
           if t1 = TInt && t2 = TInt then TInt
           else raise (TypeError ("Logical operator "^op^" requires integers"))

       | "=" | "+=" | "-=" | "*=" | "/=" | "%=" ->
           (* vérifier que e1 est une l-value et que types compatibles *)
           let is_lvalue = match e1 with
             | Id _ -> true
             | ArrayAccess(_, _) -> true
             | UnOp("*", _) -> true
             | _ -> false
           in
           if not is_lvalue then raise (TypeError "Left-hand side of assignment is not an l-value");
           if not (compatible t1 t2) then
             raise (TypeError ("Assignment operator "^op^" type mismatch"));
           t1
        | _ -> raise (TypeError ("Unknown binary operator "^op)))

   | UnOp(op, e) ->
    let t = check_expr env e in
    begin match op with
    | "++" | "--" ->
        if t = TInt then TInt
        else raise(TypeError("Unary " ^ op ^ " requires int"))
    | "post++" | "post--" ->
        if t = TInt then TInt
        else raise(TypeError("Unary " ^ op ^ " requires int"))
    | "-" ->
        if t = TInt then TInt else raise(TypeError("Unary - requires int"))
    | "!" ->
        if t = TInt then TInt else raise(TypeError("Unary ! requires int"))
    | "*" ->
        begin match t with
        | TPointer t' -> t'
        | _ -> raise(TypeError("Unary * requires pointer"))
        end
    | "&" ->
        TPointer t
    | _ ->
        raise(TypeError("Unknown unary operator " ^ op))
    end


  | Call(name, args) ->
      if not (StringMap.mem name env.funcs) then
        raise (TypeError ("Function "^name^" not declared"));
      let (ret_type, param_types) = StringMap.find name env.funcs in
      if List.length args <> List.length param_types then
        raise (TypeError ("Function "^name^" called with wrong number of arguments"));
      List.iter2 (fun arg t ->
        let arg_t = check_expr env arg in
        if not (compatible (to_simple_type t) arg_t) then
          raise (TypeError ("Function "^name^" argument type mismatch"))
      ) args param_types;
      to_simple_type ret_type

  | ArrayAccess(arr, idx) ->
      let t_arr = check_expr env arr in
      let t_idx = check_expr env idx in
      (match t_arr with
       | TPointer t when t_idx = TInt -> t
       | _ -> raise (TypeError "Array access type mismatch"))

  | SizeOf _ -> TInt

  | Cast(t, e) ->
      ignore (check_expr env e);
      to_simple_type t

(* Vérification des instructions *)
let rec check_instr env = function
  | Expr e -> ignore (check_expr env e)
  | Empty -> ()
  | Return e -> ignore (check_expr env e)

  | Block(decls, instrs) ->
      let env_block =
        List.fold_left (fun e decl ->
          match decl with
          | VarDecl(t, names) ->
              List.fold_left (fun e n -> add_var e n t) e names

          | VarDeclInit(t, name, expr) ->
              let te = check_expr e expr in
              if not (compatible (to_simple_type t) te) then
                raise (TypeError "Initializer type mismatch");
              add_var e name t

          | FunDef _ -> e
        ) env decls
      in
      List.iter (check_instr env_block) instrs

  | If(cond, then_i, else_opt) ->
      if check_expr env cond <> TInt then
        raise (TypeError "If condition must be int");
      check_instr env then_i;
      (match else_opt with Some e -> check_instr env e | None -> ())

  | While(cond, body) ->
      if check_expr env cond <> TInt then
        raise (TypeError "While condition must be int");
      check_instr env body

  | DoWhile(body, cond) ->
      check_instr env body;
      if check_expr env cond <> TInt then
        raise (TypeError "DoWhile condition must be int")

  | For(init, cond, step, body) ->
      (match init with Some e -> ignore (check_expr env e) | None -> ());
      (match cond with Some e ->
         if check_expr env e <> TInt then
           raise (TypeError "For condition must be int")
       | None -> ());
      (match step with Some e -> ignore (check_expr env e) | None -> ());
      check_instr env body

let check_types file =
  (* Étape 1 : collecter toutes les variables globales *)
  let env =
    List.fold_left (fun env decl ->
      match decl with
      | VarDecl(t, names) ->
          List.fold_left (fun e n -> add_var e n t) env names
      | VarDeclInit(t, name, expr) ->
          let te = check_expr env expr in
          if not (compatible (to_simple_type t) te) then
            raise (TypeError ("Global initializer type mismatch for "^name));
          add_var env name t
      | FunDef _ -> env
    ) empty_env file
  in
  
  (* Étape 2 : ajouter toutes les fonctions *)
  let env =
    List.fold_left (fun env decl ->
      match decl with
      | FunDef(ret, name, params, _) ->
          add_func env name ret (List.map fst params)
      | _ -> env
    ) env file
  in
  
  (* Étape 3 : vérifier les fonctions *)
  List.iter (fun decl ->
    match decl with
    | FunDef(_, _, params, body) ->
        (* Créer un environnement local pour la fonction : garder les fonctions globales, créer scope local pour vars *)
        let env_with_params =
          List.fold_left (fun e (t,n) -> 
            (* Permettre le shadowing des variables globales par les paramètres *)
            add_var_shadowing e n t
          ) { vars = StringMap.empty; funcs = env.funcs } params
        in
        let env_fun = env_with_params in
        
        (match body with
         | Block(local_decls, instrs) ->
             let env_block =
               List.fold_left (fun e decl ->
                 match decl with
                 | VarDecl(t, names) ->
                     List.fold_left (fun e n -> add_var_shadowing e n t) e names
                 | VarDeclInit(t, name, expr) ->
                     let te = check_expr e expr in
                     if not (compatible (to_simple_type t) te) then
                       raise (TypeError "Initializer type mismatch");
                     add_var_shadowing e name t
                 | FunDef _ -> e
               ) env_fun local_decls
             in
             List.iter (check_instr env_block) instrs
         | _ -> check_instr env_fun body)
    | VarDecl _ | VarDeclInit _ -> ()
  ) file;
  Printf.printf "Type checking OK !\n"