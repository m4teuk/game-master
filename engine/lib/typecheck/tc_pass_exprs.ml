type result = {
  pile_decls : (string * (string * Types.ty) list * Types.ty * Tc_ast.texpr) list;
  options_decl : (string * Types.ty * Tc_ast.texpr) list;
  top_fns : (string * Tc_ast.texpr) list;
  top_lets : (Tc_ast.tpattern * Tc_ast.texpr) list;
}

(* ---------------------------------------------------------------- *)
(* Pass B — pile visibility expressions                              *)
(* ---------------------------------------------------------------- *)

let visibility_signature : Types.ty =
  Types.T_fn ([Types.T_state; Types.T_player_id],
              Types.T_user "Visibility")

let run_visibility env errs
    (pile_decls : Tc_pass_decls.pile_decl list)
    : (string * (string * Types.ty) list * Types.ty * Tc_ast.texpr) list =
  List.map (fun (pd : Tc_pass_decls.pile_decl) ->
    (* The visibility expression sees the pile's key parameters as
       free variables — e.g. [pile Played(owner: PlayerId) ... visibility =
         fn (state, viewer) -> if_eq(owner, viewer, ...)]. Extend env
       with the params before checking. *)
    let body_env =
      List.fold_left (fun e (n, t) -> Types.add_value e n t)
        env pd.params
    in
    let vis_t =
      Tc_expr.check_expr body_env errs
        ~expected:(Some visibility_signature) pd.visibility
    in
    (pd.name, pd.params, pd.card_ty, vis_t))
    pile_decls

(* ---------------------------------------------------------------- *)
(* Pass C — function bodies                                           *)
(* ---------------------------------------------------------------- *)

(* Each fn body is checked against the declared return type, in an env
   extended with the parameter bindings. The result is wrapped in a
   synthesized [TE_lambda] so the downstream contract on [top_fns]
   (".node is always TE_lambda") holds for every entry. *)

let run_fn_bodies env errs
    (fn_decls : Tc_pass_decls.fn_decl list)
    : (string * Tc_ast.texpr) list =
  List.map (fun (fd : Tc_pass_decls.fn_decl) ->
    let body_env =
      List.fold_left (fun e (n, t) -> Types.add_value e n t)
        env fd.params
    in
    let body_t =
      Tc_expr.check_expr body_env errs ~expected:(Some fd.ret) fd.body
    in
    let lambda_ty =
      Types.T_fn (List.map snd fd.params, fd.ret)
    in
    let lambda : Tc_ast.texpr = {
      node = TE_lambda { params = fd.params; body = body_t };
      ty = lambda_ty;
      span = fd.span;
    } in
    (fd.name, lambda))
    fn_decls

(* ---------------------------------------------------------------- *)
(* Pass D — options defaults                                          *)
(* ---------------------------------------------------------------- *)

let run_options_defaults env errs
    (options_decl : Tc_pass_decls.options_field list)
    : (string * Types.ty * Tc_ast.texpr) list =
  List.map (fun (of_ : Tc_pass_decls.options_field) ->
    let default_t =
      Tc_expr.check_expr env errs ~expected:(Some of_.ty) of_.default
    in
    (of_.name, of_.ty, default_t))
    options_decl

(* ---------------------------------------------------------------- *)
(* Entry                                                              *)
(* ---------------------------------------------------------------- *)

let run (env : Types.env) (errs : Tc_errors.t)
    (decls : Tc_pass_decls.result) : Types.env * result =
  let env, top_lets = Tc_pass_lets.run env errs decls.let_decls in
  let pile_decls = run_visibility env errs decls.pile_decls in
  let top_fns = run_fn_bodies env errs decls.fn_decls in
  let options_decl = run_options_defaults env errs decls.options_decl in
  (env, { pile_decls; options_decl; top_fns; top_lets })
