open Ast
module StringMap = Map.Make(String)

(* Environnement = variables + fonctions *)
type env = {
  vars : ctype StringMap.t;           
  funcs : (ctype * ctype list) StringMap.t;  (* nom -> (ret_type, param_types) *)
}

let empty_env = {
  vars = StringMap.empty;
  funcs = StringMap.empty;
}

(* Ajouter une fonction dans l'env global *)
let add_func env name ret params =
  if StringMap.mem name env.funcs then
    failwith ("Function "^name^" already declared");
  { env with funcs = StringMap.add name (ret, params) env.funcs }

(* Ajouter une variable *)
let add_var env name typ =
  if StringMap.mem name env.vars then
    failwith ("Variable "^name^" redeclared");
  { env with vars = StringMap.add name typ env.vars }

(* Vérification des expressions *)
let rec check_expr env = function
  | Id x ->
      if not (StringMap.mem x env.vars) then
        failwith ("Variable "^x^" used but not declared")
  | Const _ -> ()
  | BinOp(_, e1, e2) ->
      check_expr env e1;
      check_expr env e2
  | UnOp(_, e) ->
      check_expr env e
  | Call(name, args) ->
      if not (StringMap.mem name env.funcs) then
        failwith ("Call to unknown function "^name);
      List.iter (check_expr env) args
  | ArrayAccess(arr, idx) ->
      check_expr env arr;
      check_expr env idx
  | SizeOf _ -> ()
  | Cast(_, e) ->
      check_expr env e
  | Parens e ->
      check_expr env e

(* Vérification des instructions *)
let rec check_instr env = function
  | Expr e ->
      check_expr env e
  | Empty -> ()
  | Return e ->
      check_expr env e
  | Block(decls, instrs) ->
      let env' =
        List.fold_left (fun env decl ->
          match decl with
          | VarDecl(t, names) ->
              List.fold_left (fun e name -> add_var e name t) env names

          | VarDeclInit(t, name, init) ->
              let e = add_var env name t in
              check_expr e init;   (* Vérification de l'expression d'initialisation *)
              e

          | FunDef _ -> env
        ) env decls
      in
      List.iter (check_instr env') instrs

  | If(cond, then_i, else_opt) ->
      check_expr env cond;
      check_instr env then_i;
      (match else_opt with
       | Some else_i -> check_instr env else_i
       | None -> ())

  | While(cond, body) ->
      check_expr env cond;
      check_instr env body

  | DoWhile(body, cond) ->
      check_instr env body;
      check_expr env cond

  | For(init, cond, step, body) ->
      (match init with Some e -> check_expr env e | None -> ());
      (match cond with Some e -> check_expr env e | None -> ());
      (match step with Some e -> check_expr env e | None -> ());
      check_instr env body

(* Vérification globale *)
let check_scope (file : decl list) =
  (* Étape 1 : pré-collecter les fonctions globales *)
  let env =
    List.fold_left (fun env decl ->
      match decl with
      | FunDef(ret_type, name, params, _) ->
          let param_types = List.map fst params in
          add_func env name ret_type param_types

      | VarDecl(t, names) ->
          List.fold_left (fun e n -> add_var e n t) env names

      | VarDeclInit(t, name, init) ->
          let e = add_var env name t in
          check_expr e init;
          e
    ) empty_env file
  in

  (* Étape 2 : vérifier chaque fonction *)
  List.iter (fun decl ->
    match decl with
    | FunDef(_, _, params, body) ->
        (* nouvel environnement pour les variables locales et paramètres *)
        let env_fun =
          List.fold_left (fun e (t,n) ->
            add_var { e with vars = StringMap.empty } n t
          ) env params
        in
        (match body with
         | Block(local_decls, instrs) -> 
             let env_block =
               List.fold_left (fun e decl ->
                 match decl with
                 | VarDecl(t, names) ->
                     List.fold_left (fun e n -> add_var e n t) e names

                 | VarDeclInit(t, name, init) ->
                     let e = add_var e name t in
                     check_expr e init;
                     e

                 | FunDef _ -> e
               ) env_fun local_decls
             in
             List.iter (check_instr env_block) instrs

         | _ -> check_instr env_fun body)

    | VarDecl _ -> ()
    | VarDeclInit _ -> ()
  ) file;

  Printf.printf "Scope checking OK !\n"
