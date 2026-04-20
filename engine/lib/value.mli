(** Runtime values, as consumed and produced by the interpreter.

    All user-visible shapes are represented structurally so equality,
    matching, and serialization have no special cases. Opaque
    language-level types ([State], [View]) wrap engine data. *)

type player_id = string
(** Transport-level identity. Rulesets only observe equality on these. *)

exception Fatal of string
(** Raised by user code via [fatal(...)] or by engine-detected
    impossibilities (empty pile on a move requiring a card, etc.).
    Lives here (not in [Interp]) so [Pile] and other runtime modules
    can raise it without a circular dependency on [Interp]. *)

(** {1 Values} *)

type t =
  | V_num of int
  | V_text of string
  | V_player of player_id
  | V_unit

  (* algebraic *)
  | V_ctor of {
      name : string;
      fields : (string * t) list;
        (** Named fields in the type's declaration order. Empty = nullary. *)
    }
  | V_tuple of t list
  | V_list of t list

  (* piles *)
  | V_pile_ref of {
      name : string;
      keys : t list;
    }

  (* functions *)
  | V_fn of closure
    (** A user-defined lambda or top-level [fn]. *)
  | V_builtin of string
    (** Name-handle for a stdlib entry in the interpreter's builtin
        table. Produced when a user references a stdlib name as a value
        (including passing it to a higher-order function). Resolved to
        the concrete [impl] at call time. *)
  | V_pile_ctor of { name : string; arity : int }
    (** Runtime-only: a parameterized pile name ([pile Hand(owner)])
        evaluated as a value. Calling it with [arity] args yields a
        [V_pile_ref]. Nullary piles are already [V_pile_ref] at
        definition and never take this form. *)
  | V_partial of {
      arity : int;
      impl : t list -> t;
    }
    (** Runtime-only: a partially-applied builtin. Used for stdlib
        functions that return a function, e.g. [owner_only(p)] which
        yields a [(State, PlayerId) -> Visibility]. [Interp.call] with
        an argument list of length [arity] invokes [impl]. *)

  (* opaque built-ins *)
  | V_state of state
  | V_view of view
  | V_rng
    (** Opaque RNG handle. Users can only pass this back into stdlib
        fns that consume it — the actual PRNG state lives in a [Rng.t
        ref] the interpreter threads through [ctx]. *)

and closure = {
  params : (string * Types.ty) list;
  body : Tc_ast.texpr;
  captured : (string * t) list;
    (** Lexical environment captured at closure construction. Top-level
        fns and lets are NOT captured here — [Interp]'s [ctx] supplies
        them at call time. *)
}

and state = {
  state_config : t;
  state_player_dicts : (player_id * t) list;
  state_piles : pile_entry list;
  state_roster : player_id list;
}

and view = {
  view_config : t;
  view_player_dicts : (player_id * t) list;
  view_piles : view_pile list;
  view_roster : player_id list;
}

and pile_entry = {
  pe_name : string;
  pe_keys : t list;
  pe_cards : t list;
    (** Top-to-bottom order: index 0 is the top card. *)
}

and view_pile = {
  vp_name : string;
  vp_keys : t list;
  vp_value : t;
    (** One of the [PileView] constructors: [Contents(items)], [Size(n)],
        or [Masked]. *)
}

(** {1 Equality} *)

val equal : t -> t -> bool
(** Structural equality for equality-admissible values (type-system §7.1).
    Raises [Invalid_argument] on functions, [State], [View], or
    [PileRef] — the caller is expected to have type-checked already. *)

(** {1 Convenience constructors for ADT values used across the engine} *)

val unit : t
val flag_on : t
val flag_off : t
val ok : t -> t
val err : t -> t
val ongoing : t
val ended : t -> t
val contents : t list -> t
val size : int -> t
val masked : t
val see_all : t
val see_size : t
val hidden : t
val lt : t

val eq_ord : t
(** The [Ordering] [EQ] constructor. Named [eq_ord] to avoid clashing
    with [equal] above. *)

val gt : t

(** {1 Destructors — used by [Engine] to surface results to the server} *)

val as_flag : t -> [ `On | `Off ] option
val as_result : t -> [ `Ok of t | `Err of t ] option
val as_game_status : t -> [ `Ongoing | `Ended of t ] option

val as_view : t -> view option
val as_state : t -> state option

(** {1 Cards} *)

type card = t
(** A card is just an ordinary value whose type matches the ruleset's
    [Card]. This alias documents intent at pile boundaries. *)
