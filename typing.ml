open Ast
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

exception TypeError of string

(* ---------- Types internes simplifiés ---------- *)

type simple_type =
  | TInt
  | TFloat
  | TVoid
  | TPointer of simple_type

let rec to_simple_type = function
  | Int | Char | Short | Long | Signed | Unsigned -> TInt
  | Float | Double -> TFloat
  | Void -> TVoid
  | Pointer t -> TPointer (to_simple_type t)

let rec simple_eq a b =
  match a, b with
  | TInt, TInt -> true
  | TFloat, TFloat -> true
  | TVoid, TVoid -> true
  | TPointer x, TPointer y -> simple_eq x y
  | _ -> false

(* compatibilité (promotion int -> float autorisée) *)
let compatible expected actual =
  simple_eq expected actual

let result_arith t1 t2 =
  match t1, t2 with
  | TFloat, _ | _, TFloat -> TFloat
  | TInt, TInt -> TInt
  | _ -> raise (TypeError "Invalid arithmetic types")

(* ---------- Environnement avec scopes ---------- *)

type env = {
  vars  : ctype StringMap.t;                      (* toutes les variables visibles *)
  funcs : (ctype * ctype list) StringMap.t;       (* nom -> (ret, params) *)
  scope : StringSet.t;                            (* noms déclarés dans le bloc courant *)
}

let empty_env = {
  vars  = StringMap.empty;
  funcs = StringMap.empty;
  scope = StringSet.empty;
}

(* ajout dans le scope courant : interdit redeclaration locale,
   mais autorise shadowing des variables de blocs extérieurs *)
let add_var env name typ =
  if StringSet.mem name env.scope then
    raise (TypeError ("Variable "^name^" redeclared"))
  else
    {
      env with
      vars  = StringMap.add name typ env.vars;
      scope = StringSet.add name env.scope;
    }

(* shadowing explicite (utilisé rarement si besoin),
   mais on garde aussi la protection "même scope interdit" *)
let add_var_shadowing env name typ =
  if StringSet.mem name env.scope then
    raise (TypeError ("Variable "^name^" redeclared"))
  else
    {
      env with
      vars  = StringMap.add name typ env.vars;
      scope = StringSet.add name env.scope;
    }

let add_func env name ret params =
  if StringMap.mem name env.funcs then
    raise (TypeError ("Function "^name^" redeclared"));
  { env with funcs = StringMap.add name (ret, params) env.funcs }

(* entrer dans un nouveau bloc : même vars/funcs, scope vidé *)
let enter_block env = { env with scope = StringSet.empty }

(* ---------- Typage des expressions ---------- *)

let rec check_expr env = function
  | Id x ->
      if not (StringMap.mem x env.vars) then
        raise (TypeError ("Variable "^x^" not declared"));
      to_simple_type (StringMap.find x env.vars)

  | Const c ->
      (match c with
       | IntConst _   -> TInt
       | FloatConst _ -> TFloat
       | StringConst _ -> TPointer TInt)  (* simplification : char* -> int* *)

  | Parens e -> check_expr env e

  | BinOp(op, e1, e2) ->
      let t1 = check_expr env e1 in
      let t2 = check_expr env e2 in
      begin match op with
      | "+" | "-" | "*" | "/" ->
          (match t1, t2 with
           | TInt, TInt
           | TFloat, TFloat
           | TInt, TFloat
           | TFloat, TInt -> ()
           | _ -> raise (TypeError "Arithmetic type mismatch"));
          result_arith t1 t2

      | "%" ->
          if t1 = TInt && t2 = TInt then TInt
          else raise (TypeError "Modulo requires integers")

      | "==" | "!=" | "<" | "<=" | ">" | ">=" ->
          if compatible t1 t2 || compatible t2 t1 then TInt
          else raise (TypeError "Comparison type mismatch")

      | "&&" | "||" ->
          if t1 = TInt && t2 = TInt then TInt
          else raise (TypeError "Logical operator requires int")

      | "=" | "+=" | "-=" | "*=" | "/=" | "%=" ->
          let is_lvalue =
            match e1 with
            | Id _ | ArrayAccess _ | UnOp("*", _) -> true
            | _ -> false
          in
          if not is_lvalue then
            raise (TypeError "Assignment to non-lvalue");
          if not (compatible t1 t2) then
            raise (TypeError "Assignment type mismatch");
          t1

      | _ ->
          raise (TypeError ("Unknown binary operator "^op))
      end

  | UnOp(op, e) ->
      let t = check_expr env e in
      begin match op with
      | "++" | "--" | "post++" | "post--" ->
          if t = TInt then TInt
          else raise (TypeError "++/-- require int")

      | "-" ->
          if t = TInt then TInt
          else raise (TypeError "Unary - requires int")

      | "!" ->
          if t = TInt then TInt
          else raise (TypeError "Unary ! requires int")

      | "*" ->
          (match t with
           | TPointer t' -> t'
           | _ -> raise (TypeError "Unary * requires pointer"))

      | "&" -> TPointer t

      | _ ->
          raise (TypeError ("Unknown unary operator "^op))
      end

  | Call(name, args) ->
    if StringMap.mem name env.vars then
        raise (TypeError ("Cannot call variable "^name));
      if not (StringMap.mem name env.funcs) then
        raise (TypeError ("Unknown function "^name));
      let (ret, params) = StringMap.find name env.funcs in
      if List.length args <> List.length params then
        raise (TypeError ("Wrong number of arguments for "^name));
      List.iter2
        (fun arg tformal ->
           let targ = check_expr env arg in
           if not (compatible (to_simple_type tformal) targ) then
             raise (TypeError ("Argument type mismatch in call to "^name)))
        args params;
      to_simple_type ret

  | ArrayAccess(arr, idx) ->
      let t_arr = check_expr env arr in
      let t_idx = check_expr env idx in
      (match t_arr, t_idx with
       | TPointer t, TInt -> t
       | _ -> raise (TypeError "Array access type mismatch"))

  | SizeOf _ -> TInt

  | Cast(t, e) ->
      ignore (check_expr env e);
      to_simple_type t

(* ---------- Typage des instructions ---------- *)

let rec check_instr ?(ret_type : simple_type option = None) env = function
  | Expr e ->
      ignore (check_expr env e)

  | Empty -> ()

  | Return e ->
      let te = check_expr env e in
      (match ret_type with
       | Some t when not (compatible t te) ->
           raise (TypeError "Return type mismatch")
       | _ -> ())

  | Block(decls, instrs) ->
      (* nouveau bloc : reset du scope, mais visibilité des vars extérieures *)
      let env_block_base = enter_block env in
      let env' =
        List.fold_left
          (fun env d ->
             match d with
             | VarDecl(t, names) ->
                 List.fold_left (fun e n -> add_var e n t) env names

             | VarDeclInit(t, n, e0) ->
                 let te = check_expr env e0 in
                 if not (compatible (to_simple_type t) te) then
                   raise (TypeError "Initializer type mismatch");
                 add_var env n t

             | FunDef _ -> env)
          env_block_base
          decls
      in
      List.iter (check_instr ~ret_type env') instrs

  | If(cond, tbranch, ebranch) ->
      if check_expr env cond <> TInt then
        raise (TypeError "If condition must be int");
      check_instr ~ret_type env tbranch;
      (match ebranch with
       | Some b -> check_instr ~ret_type env b
       | None -> ())

  | While(cond, body) ->
      if check_expr env cond <> TInt then
        raise (TypeError "While condition must be int");
      check_instr ~ret_type env body

  | DoWhile(body, cond) ->
      check_instr ~ret_type env body;
      if check_expr env cond <> TInt then
        raise (TypeError "DoWhile condition must be int")

  | For(init, cond, step, body) ->
      (match init with Some e -> ignore (check_expr env e) | None -> ());
      (match cond with
       | Some e ->
           if check_expr env e <> TInt then
             raise (TypeError "For condition must be int")
       | None -> ());
      (match step with Some e -> ignore (check_expr env e) | None -> ());
      check_instr ~ret_type env body

(* ---------- Typage global ---------- *)

let check_types (file : decl list) =
  (* PASS 1 : variables globales + signatures des fonctions *)
  let env1 =
    List.fold_left
      (fun e d ->
         match d with
         | VarDecl(t, names) ->
             List.fold_left (fun e n -> add_var e n t) e names

         | VarDeclInit(t, n, _) ->
             add_var e n t

         | FunDef(ret, name, params, _) ->
             add_func e name ret (List.map fst params))
      empty_env
      file
  in

  (* PASS 2 : initialisations globales *)
  List.iter
    (function
      | VarDeclInit(t, n, expr) ->
          let te = check_expr env1 expr in
          if not (compatible (to_simple_type t) te) then
            raise (TypeError ("Type mismatch in global initializer for "^n))
      | _ -> ())
    file;

  (* PASS 3 : corps des fonctions *)
  List.iter
    (function
      | FunDef(ret, name, params, body) ->
          (* nouvel environnement de fonction :
             - mêmes vars/funcs que env1 (globales visibles)
             - scope vidé
             - on ajoute les paramètres dans ce nouveau scope
          *)
          let env_fun_base = { env1 with scope = StringSet.empty } in
          let env_with_params =
            List.fold_left
              (fun e (t, n) -> add_var e n t)
              env_fun_base
              params
          in
          (match body with
           | Block(local_decls, instrs) ->
               (* corps de fonction = même scope que les paramètres *)
               let env_body =
                 List.fold_left
                   (fun env d ->
                      match d with
                      | VarDecl(t, names) ->
                          List.fold_left (fun e n -> add_var e n t) env names

                      | VarDeclInit(t, n, e0) ->
                          let te = check_expr env e0 in
                          if not (compatible (to_simple_type t) te) then
                            raise (TypeError "Initializer type mismatch");
                          add_var env n t

                      | FunDef _ -> env)
                   env_with_params
                   local_decls
               in
               List.iter
                 (check_instr ~ret_type:(Some (to_simple_type ret)) env_body)
                 instrs

           | _ ->
               (* cas où body n'est pas un Block (rare avec ton AST) *)
               check_instr
                 ~ret_type:(Some (to_simple_type ret))
                 env_with_params
                 body)

      | _ -> ())
    file;

  Printf.printf "Type checking OK!\n"