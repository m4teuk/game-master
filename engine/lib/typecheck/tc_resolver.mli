(** Resolves an [Ast.type_expr] to a [Types.ty].

    Recognizes built-in non-generic and generic types, looks up user
    type names in [env], and rejects type variables in user
    declarations (type-system §2). Wrong arities and unknown names are
    reported via [Tc_errors]; the function returns a placeholder so
    callers can continue. *)

val resolve_ty : Types.env -> Tc_errors.t -> Ast.type_expr -> Types.ty
