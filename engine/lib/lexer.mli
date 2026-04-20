(** Pass 1 of load: [string -> Token.positioned list].

    The lexer is total over valid UTF-8 input, but returns [Load_error.t] for
    invalid encoding, unterminated text literals, bad escape sequences, or
    unexpected characters. *)

val tokenize : file:string -> string -> (Token.positioned list, Load_error.t) result
(** [tokenize ~file source] produces the token stream for [source].
    The [file] argument is threaded into every token span so later error
    messages can reference it.

    The returned list ends with exactly one [Token.EOF]. *)
