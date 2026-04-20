(** Line-based I/O over a single TCP connection. Writes are serialised
    through a per-conn mutex so broadcasts and targeted replies from
    different threads cannot interleave. *)

type conn

val make : Unix.file_descr -> Unix.sockaddr -> conn

val peer : conn -> string
(** Human-readable peer address, for logging. *)

val write_line : conn -> string -> unit
(** Write [s] followed by a newline. *)

val write_block : conn -> string -> unit
(** Write [s] as-is, appending a newline only if [s] does not already
    end with one. Intended for multi-line payloads like rendered views. *)

val read_line : conn -> string option
(** [None] on EOF or socket error. *)

val close : conn -> unit
(** Idempotent. Shuts down both halves of the socket. *)

val is_closed : conn -> bool
