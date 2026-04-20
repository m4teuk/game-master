(** Error accumulator for typecheck.

    Per type-system §10, the checker reports {i all} errors found rather
    than failing fast. Helpers throughout the typecheck library push
    into a shared accumulator and return placeholders so traversal
    continues. *)

type t

val create : unit -> t

val report : t -> Load_error.span -> string -> unit
(** Records a [Type]-category error at the given span. *)

val collected : t -> Load_error.t list
(** All errors reported so far, in source order (oldest first). *)

val is_empty : t -> bool
