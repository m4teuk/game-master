(** Default implementations of [builtin_action_to_text],
    [builtin_view_to_text], [builtin_outcome_to_text],
    [builtin_text_to_action], plus the JSON variants (stdlib §14).

    Kept in a dedicated module because the generic rendering logic for
    algebraic values is non-trivial — it walks the [Link.t] to discover
    ctor names and field layouts, which would otherwise pollute
    [Stdlib_impl]. *)

val action_to_text : Link.t -> Value.t -> Value.player_id -> string
val view_to_text : Link.t -> Value.view -> Value.player_id -> string
val outcome_to_text : Link.t -> Value.t -> Value.player_id -> string

val text_to_action :
  Link.t ->
  string ->
  view:Value.view ->
  player:Value.player_id ->
  (Value.t, string) result
(** Parses the text produced by [action_to_text] back into an [Action]
    value. Round-trip: for any action [a] whose type matches the
    ruleset's [Action], [text_to_action l (action_to_text l a p) ~view ~player = Ok a]
    (stdlib §14). *)

val action_to_json : Link.t -> Value.t -> Value.player_id -> string
val view_to_json : Link.t -> Value.view -> Value.player_id -> string
val outcome_to_json : Link.t -> Value.t -> Value.player_id -> string
val json_to_action :
  Link.t ->
  string ->
  view:Value.view ->
  player:Value.player_id ->
  (Value.t, string) result
