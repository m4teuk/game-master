(** Exhaustiveness check for [match] expressions (type-system §6.2).

    Per-type case analysis: ADT scrutinees must cover every
    constructor; Num/Text scrutinees must end in a wildcard or
    binding pattern; List scrutinees must cover [[]] and non-empty;
    tuple scrutinees must cover the shape; opaque scrutinees are
    rejected outright (no patterns are valid).

    Also emits unreachable-arm warnings (§6.3).

    Kept as a separate module from [Tc_expr] because the algorithm is
    self-contained and the file is expected to grow (matrix-style
    checks for nested patterns may eventually replace the v0 per-type
    case analysis). *)

val check :
  Types.env ->
  Tc_errors.t ->
  scrutinee_ty:Types.ty ->
  scrutinee_span:Ast.span ->
  Ast.pattern list ->
  unit
