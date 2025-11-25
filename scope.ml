open Ast
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

exception Scope_error of string

(* Environnement = variables + fonctions + scope courant *)
type env = {
  vars  : ctype StringMap.t;                 (* nom -> type des variables visibles *)
  funcs : (ctype * ctype list) StringMap.t;  (* nom -> (ret_type, param_types) *)
  scope : StringSet.t;                       (* noms déclarés dans le bloc courant *)
}

let empty_env = {
  vars  = StringMap.empty;
  funcs = StringMap.empty;
  scope = StringSet.empty;
}

(* Ajoute une variable dans le scope courant.
   Interdit la redéclaration dans le même bloc,
   MAIS autorise le shadowing de variables des blocs extérieurs. *)
let add_var env name typ =
  if StringSet.mem name env.scope then
    raise (Scope_error ("Variable "^name^" redeclared"))
  else
    {
      env with
      vars  = StringMap.add name typ env.vars;
      scope = StringSet.add name env.scope;
    }

(* Variante "shadowing" : autorise de remplacer un ancien type
   dans vars, mais toujours interdit dans le même scope.
   On va l'utiliser pour les paramètres si besoin, mais en pratique
   add_var suffit si on réinitialise bien scope au début de la fonction. *)
let add_var_shadowing env name typ =
  if StringSet.mem name env.scope then
    raise (Scope_error ("Variable "^name^" redeclared"))
  else
    {
      env with
      vars  = StringMap.add name typ env.vars;
      scope = StringSet.add name env.scope;
    }

(* Ajoute une fonction globale *)
let add_func env name ret_type param_types =
  if StringMap.mem name env.funcs then
    raise (Scope_error("Function "^name^" redeclared"));
  { env with funcs = StringMap.add name (ret_type, param_types) env.funcs }

(* Vérification des expressions (contrôles basiques) *)
let rec check_expr env = function
  | Id x ->
      if not (StringMap.mem x env.vars) then
       raise(Scope_error("Variable "^x^" used but not declared"))
  | Const _ -> ()
  | BinOp(_, e1, e2) ->
      check_expr env e1;
      check_expr env e2
  | UnOp(_, e) ->
      check_expr env e
  | Call(name, args) ->
    if not (StringMap.mem name env.funcs) then
        raise(Scope_error("Call to unknown function "^name));
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
      (* Nouveau bloc : même vars (on voit les variables extérieures),
         mais scope réinitialisé (noms déclarés dans CE bloc uniquement). *)
      let env_block_base = { env with scope = StringSet.empty } in
      let env_local =
        List.fold_left (fun e decl ->
          match decl with
          | VarDecl(t, names) ->
              List.fold_left (fun e name -> add_var e name t) e names

          | VarDeclInit(t, name, init) ->
              let e = add_var e name t in
              check_expr e init;   (* Vérification de l'expression d'init *)
              e

          | FunDef _ -> e
        ) env_block_base decls
      in
      List.iter (check_instr env_local) instrs

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

(* Vérification globale du fichier (liste de déclarations) *)
(*let check_scope (file : decl list) =
  (* Étape 1 : pré-collecter les signatures des fonctions *)
  let env_with_funcs =
    List.fold_left (fun env decl ->
      match decl with
      | FunDef(ret_type, name, params, _) ->
          let param_types = List.map fst params in
          add_func env name ret_type param_types
      | _ -> env
    ) empty_env file
  in

  (* Étape 2 : ajouter les variables globales (et vérifier les initialisations) *)
  let env_globals =
    List.fold_left (fun env decl ->
      match decl with
      | VarDecl(t, names) ->
          List.fold_left (fun e n -> add_var e n t) env names

      | VarDeclInit(t, name, init) ->
          let e = add_var env name t in
          (* on veut avoir aussi toutes les fonctions visibles pour init *)
          let e_for_init = { e with funcs = env_with_funcs.funcs } in
          check_expr e_for_init init;
          e

      | FunDef _ -> env
    ) env_with_funcs file
  in

  (* Étape 3 : vérifier chaque fonction (corps) *)
  List.iter (fun decl ->
    match decl with
    | FunDef(ret_type, name, params, body) ->
        (* nouvel environnement pour la fonction :
           - mêmes vars que globals (elles sont visibles)
           - scope RÉINITIALISÉ (nouveau scope de fonction)
           - on ajoute les paramètres dans ce scope *)
        let env_fun_base = { env_globals with scope = StringSet.empty } in
        let env_fun =
          List.fold_left (fun e (t,n) -> add_var e n t) env_fun_base params
        in
        (match body with
         | Block(local_decls, instrs) ->
             (* le bloc du corps de fonction est dans le même scope que les params
                et les premières déclarations locales *)
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

         | _ ->
             (* cas théorique si body n'est pas un Block *)
             check_instr env_fun body)

    | VarDecl _ -> ()
    | VarDeclInit _ -> ()
  ) file;

  Printf.printf "Scope checking OK !\n"*)
  let check_scope (file : decl list) =
  (* Traiter les déclarations dans l'ordre, sans pré-collecte *)
  let rec check_decls env = function
    | [] -> ()
    | decl :: rest ->
        match decl with
        (* Variables globales *)
        | VarDecl(t, names) ->
            let env' = List.fold_left (fun e n -> add_var e n t) env names in
            check_decls env' rest
        
        | VarDeclInit(t, name, init) ->
            check_expr env init;  (* Vérifier avec l'env ACTUEL *)
            let env' = add_var env name t in
            check_decls env' rest
        
        (* Fonctions : ajouter la signature PUIS vérifier le corps *)
        | FunDef(ret_type, name, params, body) ->
            let param_types = List.map fst params in
            let env' = add_func env name ret_type param_types in
            
            (* Vérifier le corps avec l'environnement qui contient la fonction *)
            let env_fun = { env' with scope = StringSet.empty } in
            let env_with_params =
              List.fold_left (fun e (t,n) -> add_var e n t) env_fun params
            in
            (match body with
             | Block(local_decls, instrs) ->
                 let env_body =
                   List.fold_left (fun e d ->
                     match d with
                     | VarDecl(t, names) ->
                         List.fold_left (fun e n -> add_var e n t) e names
                     | VarDeclInit(t, name, init) ->
                         check_expr e init;
                         add_var e name t
                     | FunDef _ -> e
                   ) env_with_params local_decls
                 in
                 List.iter (check_instr env_body) instrs
             | _ -> check_instr env_with_params body);
            
            (* Continuer avec l'environnement enrichi *)
            check_decls env' rest
  in
  check_decls empty_env file;
  Printf.printf "Scope checking OK !\n"