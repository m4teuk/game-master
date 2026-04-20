(** Pass A: collect top-level declarations into the type environment.

    Walks [Ast.file] and registers user types, pile names, the options
    block, function signatures, and top-level lets. Bodies (function
    bodies, visibility expressions, options defaults, top-level let
    RHS) are {b not} checked here — they are deferred to
    [Tc_pass_exprs] so that all top-level names are visible to all
    other declarations (type-system §8.3).

    Two-step within types: first register all type names so mutually
    recursive type definitions resolve correctly (type-system §3),
    then resolve each type's constructor field types.

    Errors are pushed via [Tc_errors]; the returned [env] always has
    {i some} binding for every successfully named declaration so later
    passes can keep checking. *)

type pile_decl = {
  name : string;
  params : (string * Types.ty) list;
  card_ty : Types.ty;
  visibility : Ast.expr;
  span : Ast.span;
}

type options_field = {
  name : string;
  ty : Types.ty;
  default : Ast.expr;
  span : Ast.span;
}

type fn_decl = {
  name : string;
  params : (string * Types.ty) list;
  ret : Types.ty;
  body : Ast.expr;
  span : Ast.span;
}

type let_decl = {
  pat : Ast.pattern;
  body : Ast.expr;
  span : Ast.span;
}

type result = {
  env : Types.env;
  type_decls : (string * Types.type_info) list;
  pile_decls : pile_decl list;
  options_decl : options_field list;
  fn_decls : fn_decl list;
  let_decls : let_decl list;
}

val run : Types.env -> Tc_errors.t -> Ast.file -> result
