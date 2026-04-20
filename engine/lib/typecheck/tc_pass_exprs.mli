(** Passes E/B/C/D: drive [Tc_expr] over the deferred expression slots
    collected by Pass A.

    Ordering matters: type-system §8.3 makes all top-level names
    visible to all others, so a function body, visibility expression,
    or options default may reference a top-level let. Lets have no
    annotation, so their types must be inferred {b before} any other
    deferred body is checked.

    - {b Pass E} (top-level lets, in [Tc_pass_lets]): infer types of
      pattern variables from each let body, then add to env. Detects
      value-level cycles (a let that depends on itself directly or
      transitively) and collisions against stdlib/piles/fns. After
      this, [env.values] contains every top-level value-level binding.
    - {b Pass B} (visibility): each pile's visibility expression is
      checked against [(State, PlayerId) -> Visibility].
    - {b Pass C} (function bodies): each [fn_decl]'s body is checked
      against the declared return type, in an env extended with
      parameters. Each entry in [top_fns] is a synthesized [TE_lambda]
      so downstream consumers see a uniform shape.
    - {b Pass D} (options defaults): each [options_field]'s default is
      checked against the field's declared type. Per type-system §9 the
      default is later evaluated once, at engine-form-generation time. *)

type result = {
  pile_decls : (string * (string * Types.ty) list * Types.ty * Tc_ast.texpr) list;
  options_decl : (string * Types.ty * Tc_ast.texpr) list;
  top_fns : (string * Tc_ast.texpr) list;
  top_lets : (Tc_ast.tpattern * Tc_ast.texpr) list;
}

val run :
  Types.env -> Tc_errors.t -> Tc_pass_decls.result ->
  Types.env * result
(** Returns the final env (extended with let bindings) alongside the
    typed expression results. Type declarations themselves are already
    final after Pass A and are not routed through here — [Typecheck.check]
    reads them straight from the [Tc_pass_decls.result]. *)
