(** The {b public} entry point for the [.game] runtime engine.

    A host server uses only the identifiers exported from this module.
    Every other module in the library is an implementation detail.

    {1 Lifecycle}

    {ol
    {- Call [parse] once per ruleset version. The result is reusable across
       many game sessions.}
    {- For each new session, call [options_form] to know which fields to
       prompt the operator for, then [init_state] to produce an initial
       [engine_state].}
    {- On every player input, call [apply]. It returns the updated state
       plus the rendered action string to broadcast.}
    {- After each successful [apply], call [status] per player to learn
       whether the game has ended and, if so, the personalized outcome
       text.}
    {- Between actions, call [display] per player to render a fresh view
       of the state.}
    {- [log] exposes the full history of validated actions for replay or
       display.}
    } *)

(** {1 Types} *)

type parsed
(** A fully loaded, type-checked, linked ruleset. Immutable. Thread-safe
    for concurrent reads across sessions. *)

type engine_state
(** A single in-flight game session: the ruleset's [State] plus engine-
    owned pieces — RNG, action log, player roster, and the
    pre-rendered transcript. Immutable; each call returns a fresh one. *)

type player = string
(** Transport-level player identity. The engine treats these opaquely;
    equality is string equality. *)

(** {2 Options schema (stdlib §14 / type-system §9)}

    The options {i schema} is the shape the ruleset declares; the
    options {i value} is what the server supplies to [init_state]. Both
    are structured so the server can prompt per-field without doing its
    own type acrobatics. *)

type option_type =
  | OT_num
  | OT_text
  | OT_enum of string list
    (** User-declared type whose constructors are all nullary. The
        strings are the constructor names, in declaration order. *)

type option_value =
  | OV_num of int
  | OV_text of string
  | OV_enum of string
    (** The chosen constructor name; must appear in the matching
        [OT_enum] schema. *)

type option_field = {
  name : string;
  ty : option_type;
  default : option_value;
    (** Evaluated once, at [parse] time, against the ruleset's declared
        default expression. *)
}

(** {2 Per-player status} *)

type status =
  | Ongoing
  | Ended of string
    (** The outcome rendered for the querying player via
        [outcome_to_text]. *)

(** {2 Action log entries} *)

type log_entry = {
  player : player;
    (** The player who submitted the action. *)
  rendered : string;
    (** The text form produced by the ruleset's [action_to_text] at the
        moment the action was applied. Stored here so the transcript is
        stable even if a future engine change would alter rendering. *)
}

(** {2 Errors}

    Three distinct error surfaces, matching runtime.md §8.1. They are
    deliberately {i not} unified — servers route load errors to
    operators, setup errors to the setup UI, and apply errors either
    back to the acting player (invalid) or to an incident channel
    (fatal). *)

type setup_error =
  | Invalid_options of string
    (** Supplied [options] does not match the schema: missing fields,
        unexpected fields, type mismatch, enum variant not declared. *)
  | Invalid_seed of string
    (** The supplied [seed] was not the required 16 bytes. Distinct
        from [Invalid_options] so servers can route seed-generation
        bugs separately from operator-facing option errors. *)
  | Invalid_players of string
    (** The engine rejected the roster (empty or contains duplicates)
        before calling [setup]. *)
  | Setup_rejected of string
    (** [setup] returned [Err(msg)]. A configuration-level user error. *)
  | Setup_fatal of string
    (** [setup] called [fatal(...)]. A ruleset bug; includes enough
        context for a crash report. *)

type apply_error =
  | Invalid of string
    (** Parse ([text_to_action]) or validate ([validate]) failed. Safe
        to show to the acting player; never logged. *)
  | Fatal of string
    (** [apply] called [fatal(...)] or the engine detected an
        impossibility. Per runtime.md §8.1, the session is no longer
        usable after this; the server should surface a crash dump and
        tear down. *)

(** {1 Loading a ruleset} *)

val parse : source_name:string -> string -> (parsed, Load_error.t list) result
(** [parse ~source_name src] runs the full load pipeline (lex → parse →
    typecheck → link).

    Returns every error found. The list is nonempty on failure. Only
    the [Type] pass batches: [Lex], [Parse], and [Link] each stop at
    their first error and so always contribute a singleton list when
    they fail. The uniform [Load_error.t list] surface lets callers
    handle all load-time failures the same way.

    [source_name] is carried into all [Load_error.t] spans. *)

val source_name : parsed -> string

(** {1 Introspecting the ruleset} *)

val options_form : parsed -> option_field list
(** Empty list means the ruleset has no [options {}] block — the server
    should pass [options:[]] to [init_state]. *)

(** {1 Starting a session} *)

val init_state :
  parsed ->
  options:(string * option_value) list ->
  players:player list ->
  seed:bytes ->
  (engine_state, setup_error) result
(** [seed] must be exactly 16 bytes (128 bits). Any other length yields
    [Invalid_seed _].

    [options] is order-insensitive but must name every field from
    [options_form] exactly once. *)

(** {1 Driving the session}

    These functions take [parsed] explicitly rather than bundling it
    into [engine_state] so a ruleset can back many concurrent sessions
    without cloning its immutable parts. *)

val validate :
  parsed ->
  engine_state ->
  player:player ->
  input:string ->
  (unit, apply_error) result
(** Dry-run: parse [input] as an [Action] and run [validate] against the
    acting player's view. Does not mutate state.

    The error discriminant matches [apply]'s: [Invalid _] for a user-
    facing message, [Fatal _] for a ruleset bug that crashed during
    parse or validate. If [player] is not in [players], returns
    [Error (Invalid "no such player")]. *)

val apply :
  parsed ->
  engine_state ->
  player:player ->
  input:string ->
  (engine_state * string, apply_error) result
(** The full action loop (runtime.md §3.2, steps 2–5):

    {ol
    {- parse [input] to an [Action] via [text_to_action];}
    {- [validate] against the player's view;}
    {- advance [State] via [apply];}
    {- append the rendered action to the log.}}

    On success, returns [(state', rendered_action)]. The server
    broadcasts [rendered_action] to all players (runtime.md §3.2 step 6)
    and queries [display] / [status] per player afterward. *)

val display : parsed -> engine_state -> player:player -> string
(** Renders [view_to_text(view(state, player), player)]. If [player] is
    not in the roster, returns a short error string (" <no such player>"),
    never raises. *)

val status : parsed -> engine_state -> player:player -> status
(** Calls [terminal(state)] and, if ended, renders the outcome for
    [player]. The outcome is player-personalized (type-system §3.3):
    different viewers can see different text. *)

(** {1 Session metadata} *)

val players : engine_state -> player list
(** Returns the roster in seat order, exactly as passed to
    [init_state]. Immutable for the session (runtime.md §2). *)

val log : engine_state -> log_entry list
(** All validated actions, oldest first. Stable across state-observing
    calls — only [apply] ever appends to it. *)

val seed : engine_state -> bytes
(** The 16-byte seed originally passed to [init_state]. Retained so the
    server can persist a replay record
    [(source_name, seed, players, options, log, final_state)]. *)

val options : engine_state -> (string * option_value) list
(** The options the session was initialized with (post-validation, after
    any defaults were filled in). Same order as [options_form parsed]. *)

(** {1 Re-exported modules}

    Exposed as [Error] (rather than [Load_error]) so external callers
    write [Engine.Error.t] — a stable, short public surface, independent
    of the internal module rename done to avoid collision with
    [Core.Error]. *)

module Error : module type of Load_error

(** {1 Development access}

    [Dev] re-exports internal modules so dev tooling (e.g. the CLI
    inspector under [bin/]) can pretty-print intermediate stages. {b Not
    part of the supported runtime API.} Production hosts should never
    reach for [Dev]; signatures inside it can change between minor
    versions without notice. *)

module Dev : sig
  module Load_error : module type of Load_error
  module Token      : module type of Token
  module Lexer      : module type of Lexer
  module Parser     : module type of Parser
  module Ast        : module type of Ast
  module Types      : module type of Types
  module Tc_ast     : module type of Tc_ast
  module Typecheck  : module type of Typecheck
  module Link       : module type of Link
  module Value      : module type of Value
  module Rng        : module type of Rng
  module Pile       : module type of Pile
  module Interp     : module type of Interp

  val raw_state : engine_state -> Value.state
  (** Dev-only peek at the unmasked game state. Production code should
      never call this — it bypasses the view-masking contract. *)
end
