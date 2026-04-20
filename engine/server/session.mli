(** A running game session. Owns the engine state, a fixed roster of
    player slots, and a driver thread that serialises actions FIFO.

    Used by both [simple_server] and [server]. *)

type t

val create :
  parsed:Engine.parsed ->
  initial:Engine.engine_state ->
  on_game_end:(unit -> unit) ->
  t
(** Build a session around a ready [engine_state]. Spawns the driver
    thread immediately; the session waits for connections via [attach].
    [on_game_end] fires exactly once, after terminal/fatal/timeout
    broadcasts have been written and all connections closed. *)

val attach : t -> Engine.player -> Protocol.conn -> (unit, string) result
(** Bind [conn] to the named player slot.

    - If [player] is not in the roster, returns [Error _] and the
      caller disconnects.
    - If the slot already has a live connection, that previous
      connection is displaced (sent a notice and closed), and the new
      one takes over. This is the reconnect path.
    - When the final slot fills for the first time, the session
      transitions to [Running] and broadcasts "game starting" + views.
    - During [Running], a fresh attach immediately renders the current
      view to the new conn.

    Spawns a reader thread that pushes the client's lines onto the
    session's action queue. *)

val has_any_connected : t -> bool
(** Used by the [server] reaper: a session with no connected players
    for longer than the timeout is shut down. *)

val shutdown : t -> reason:string -> unit
(** Broadcast [reason], close every connection, invoke [on_game_end].
    Idempotent. Used by the reaper and by admin-style kill paths. *)
