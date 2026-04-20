(** Tree-walking evaluator for the typed AST.

    The interpreter is stateless w.r.t. anything outside its [ctx]; it
    threads RNG and pile registry updates explicitly. Ruleset-triggered
    [fatal(...)] is raised as an OCaml exception ([Fatal]) and caught by
    the [Engine] entry points. *)

(** {1 Exceptions} *)

exception Fatal of string
(** Alias for [Value.Fatal] — the same exception, exposed under the
    [Interp] name for backward compatibility with the original contract.
    Catching one catches the other. *)

(** {1 Capabilities} *)

type capability =
  | Cap_setup
  | Cap_apply
  | Cap_validate
  | Cap_terminal
  | Cap_visibility
  | Cap_action_to_text
  | Cap_text_to_action
  | Cap_view_to_text
  | Cap_outcome_to_text
  | Cap_toplevel

(** {1 Contexts} *)

type ctx

(** {1 Builtins} *)

type builtin = {
  name : string;
  capabilities : capability list;
    (** Contexts in which this built-in is callable. Empty means "any
        context." A call from a disallowed context raises [Fatal]. *)
  impl : ctx -> Value.t list -> Value.t;
}

(** {1 Context construction} *)

val make_ctx :
  link:Link.t ->
  builtins:builtin list ->
  toplevel:(string * Value.t) list ->
  capability:capability ->
  roster:Value.player_id list ->
  rng:Rng.t ref ->
  temp_scope:Pile.scope option ->
  ctx

val build_toplevel :
  Link.t -> builtin list -> (string * Value.t) list
(** Run at [Engine.parse] time. Builds a [V_fn] closure for every user
    fn, a [V_pile_ref] or [V_pile_ctor] for every pile, then evaluates
    each top-level let in topo order under [Cap_toplevel]. The returned
    list is the canonical top-level environment — pass it to every
    subsequent [make_ctx] for the session. *)

(** {1 Evaluation} *)

val eval : ctx -> Typecheck.texpr -> Value.t

val call : ctx -> Value.t -> Value.t list -> Value.t
(** Applies a [V_fn] / [V_builtin] / [V_pile_ctor] to the given
    arguments. Raises [Fatal] on arity mismatch, disallowed capability,
    or a non-callable value (any of these would be a typechecker bug
    if reached at runtime). *)

(** {1 Context accessors — for builtin implementations} *)

val ctx_link : ctx -> Link.t
val ctx_rng : ctx -> Rng.t ref
val ctx_temp_scope : ctx -> Pile.scope option
val ctx_capability : ctx -> capability
val ctx_toplevel : ctx -> (string * Value.t) list
val ctx_roster : ctx -> Value.player_id list

val extend_locals : ctx -> (string * Value.t) list -> ctx
(** Returns a new [ctx] with the given bindings prepended to [locals].
    Used by the engine when evaluating per-instance pile visibility —
    the pile's key parameters ([owner] in [pile Hand(owner)]) need to
    be in scope for the visibility expression. *)
