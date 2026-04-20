(** Live-game registry for the multi-game [server] binary.

    Each running session is keyed by a short opaque [game_id]. A
    background reaper thread cleans up sessions whose roster has sat
    empty longer than the configured timeout. *)

type entry = {
  game_id : string;
  session : Session.t;
  mutable empty_since : float option;
}

type t

val create : unit -> t

val gen_id : t -> string
(** Returns a short unused game id. *)

val add : t -> entry -> unit
val find : t -> string -> entry option
val remove : t -> string -> unit
val ids : t -> string list

val start_reaper : t -> timeout:float -> unit
(** Spawns a background thread that shuts down and removes any session
    with no connected players for longer than [timeout] seconds. *)
