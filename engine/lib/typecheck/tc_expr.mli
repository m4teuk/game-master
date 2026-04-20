(** Pass C core: type-check expressions and patterns.

    Used by [Tc_pass_exprs] to check function bodies, pile visibility
    expressions, options-field defaults, and top-level let RHSs. The
    public [Typecheck] module never calls into this directly.

    Bidirectional checking: [check_expr ~expected:(Some t)] propagates
    a known type {i down} (needed for empty lists, lambdas in
    higher-order calls, etc.); [~expected:None] runs in synthesis mode.

    Pattern checking takes a known scrutinee type and returns the
    typed pattern plus the value-level bindings it introduces. *)

val check_expr :
  Types.env ->
  Tc_errors.t ->
  expected:Types.ty option ->
  Ast.expr ->
  Tc_ast.texpr

val check_pattern :
  Types.env ->
  Tc_errors.t ->
  expected:Types.ty ->
  Ast.pattern ->
  Tc_ast.tpattern * (string * Types.ty) list
