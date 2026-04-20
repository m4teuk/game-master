(** Load-time errors: produced by lex, parse, type-check, and link stages.

    Runtime errors (setup/apply) use the bespoke error types in [Engine],
    because the server routes them differently from load-time errors. *)

type span = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

val no_span : span
(** Sentinel used for synthesized nodes that have no source location. *)

type category =
  | Lex
  | Parse
  | Type
  | Link

type t = {
  category : category;
  span : span;
  message : string;
}

val make : category -> span -> string -> t

val to_string : t -> string
(** Renders as ["<file>:<line>:<col>: <category>: <message>"]. *)
