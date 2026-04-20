(** Pass E (structural + inference) for top-level lets.

    Three structural checks first, each pushing into [Tc_errors]:
    - {b Collision}: each pattern-bound variable must not collide with
      any [env.values] entry (stdlib, pile, fn) or with a variable
      bound by another top-level let.
    - {b Irrefutability}: each let pattern must be irrefutable per
      type-system §6.5.
    - {b Cycle detection}: walks each let RHS to gather references to
      other top-level let names, builds a dep graph, and reports every
      let that participates in a value-level cycle (§8.3).

    Then inference: in topological order (dependencies first), each
    let body is type-checked, then matched against its pattern to
    derive the bound variables' types, which extend the env for
    subsequent lets and for Passes B/C/D.

    Returns the extended env and a [(tpattern, texpr)] entry per
    successfully-typed let, in source order. *)

val run :
  Types.env -> Tc_errors.t ->
  Tc_pass_decls.let_decl list ->
  Types.env * (Tc_ast.tpattern * Tc_ast.texpr) list
