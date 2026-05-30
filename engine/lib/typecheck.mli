(** Pass 3 of load: resolves names, checks types, verifies exhaustiveness.

    Produces a typed AST (every node carries an inferred [ty]) plus the
    populated type environment that [Link], [Render], and [Interp] share.

    {1 Module organization}

    The implementation lives in private sub-modules under
    [lib/typecheck/]:
    {ul
    {- [Tc_ast] — the typed-AST type definitions (re-exported below).}
    {- [Tc_errors] — push-and-continue error accumulator (§10).}
    {- [Tc_resolver] — [Ast.type_expr] to [Types.ty].}
    {- [Tc_pass_decls] — Pass A: collect top-level declarations.}
    {- [Tc_expr] — expression and pattern checking core.}
    {- [Tc_pass_exprs] — Passes E/B/C/D: drive [Tc_expr] over deferred
       expressions, in dependency order.}}

    Required-declaration and required-signature audits (type-system
    §3.2 / §3.3) are {b not} done here — they live in [Link], whose
    error category is file-level rather than expression-level.

    None of the [Tc_*] sub-modules are part of the engine's public
    API; only the declarations in this module are. *)

(** {1 Typed AST}

    Re-exported from [Tc_ast] as type-equality aliases so external code
    pattern-matches on the concrete shape. [tfile] is abstract: its
    internal indexing is not part of the contract; downstream modules
    use the accessors below. *)

type texpr = Tc_ast.texpr = {
  node : texpr_node;
  ty : Types.ty;
  span : Ast.span;
}

and texpr_node = Tc_ast.texpr_node =
  | TE_num of int
  | TE_text of string
  | TE_var of string
    (** [VALUE_IDENT]. Resolution order at eval time: locals, then
        top-level bindings (fns, lets), then stdlib builtins. *)
  | TE_ctor of string * (string * texpr) list
    (** Constructor application. Field list is [] for nullary
        constructors. Fields are stored in the type's declaration
        order, not source order — keyword/punning resolution happens
        at check time. *)
  | TE_record_update of {
      ctor : string;                                   (** the sole record ctor *)
      spread : texpr;
      fields : (string * texpr) list;                  (** overrides, in declaration order *)
    }
  | TE_tuple of texpr list
  | TE_list of texpr list
  | TE_app of texpr * texpr list
    (** Arguments in parameter-declaration order. Keyword arguments in
        the surface syntax are reordered to positional by the checker. *)
  | TE_let of { pat : tpattern; value : texpr; body : texpr }
    (** The LHS is always an irrefutable pattern — refutable patterns on
        [let] bindings are rejected in [Typecheck]. *)
  | TE_match of { scrutinee : texpr; arms : (tpattern * texpr) list }
  | TE_lambda of {
      params : (string * Types.ty) list;               (** annotations filled in by inference *)
      body : texpr;
    }
  | TE_bin of Ast.bin_op * texpr * texpr
  | TE_rel of Ast.rel_op * texpr * texpr
    (** Relational operator on [Num]s: [a < b], [a == b], etc. Result
        is a [Flag]. *)
  | TE_if of { cond : texpr; then_ : texpr; else_ : texpr }
    (** [if cond then a else b]. [cond] has type [Flag]; [a] and [b]
        have the same type (the result). Evaluation is lazy — only the
        taken branch is evaluated. *)
  | TE_neg of texpr

and tpattern = Tc_ast.tpattern = {
  pat_node : tpattern_node;
  pat_ty : Types.ty;
  pat_span : Ast.span;
}

and tpattern_node = Tc_ast.tpattern_node =
  | TP_wild
  | TP_var of string
  | TP_num of int
  | TP_ctor of {
      name : string;
      fields : (string * tpattern) list;
        (** [] for nullary constructors. Punning is resolved here:
            [Card { rank }] becomes [("rank", TP_var "rank")]. *)
      has_rest : bool;
    }
  | TP_tuple of tpattern list
  | TP_list_exact of tpattern list                     (** [[]] or [[p1, ..., pn]] *)
  | TP_list_cons of {
      heads : tpattern list;
      rest : string option;                            (** [None] for anonymous [..] *)
    }

type tfile

val expr_type : texpr -> Types.ty
(** Convenience alias for [e.ty]. *)

(** {1 Entry point} *)

val check : Ast.file -> (tfile * Types.env, Load_error.t list) result
(** Collects {i all} type errors, not just the first, so the caller can
    surface them as a batch. Returns [Ok (tfile, env)] only if the file
    is fully well-typed. *)

(** {1 Views of the typed AST}

    The intent is that downstream modules work entirely through these
    accessors; they should not need to pattern-match against [texpr]
    directly. *)

val top_fns : tfile -> (string * texpr) list
(** Each entry is [(name, lambda_expr)] where [lambda_expr.node] is
    always [TE_lambda]. *)

val top_lets : tfile -> (tpattern * texpr) list
(** Top-level constants, already cycle-checked. Each entry is the
    [(pat, value)] pair from a [let pat = value] declaration; for
    multi-bind patterns (e.g. [let (h1, h2) = split_at(xs, 26)]) the
    runtime destructures [pat] against the value at session start. *)

val type_decls : tfile -> (string * Types.type_info) list

val pile_decls :
  tfile ->
  (string * (string * Types.ty) list * Types.ty * texpr) list
(** [(name, key_params, card_ty, visibility_expr)]. *)

val options_decl :
  tfile ->
  (string * Types.ty * texpr) list
(** Each field: [(name, type, default_expr)]. Empty list means the file
    has no [options] block (so [Options] aliases [Unit]). *)

val source_name : tfile -> string
