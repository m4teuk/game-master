(** Pass 2 of load: [Token.positioned list -> Ast.file].

    The parser enforces grammar.md §2 exactly. It does not resolve names,
    check types, or verify that required declarations are present — those
    belong to [Typecheck] and [Link]. *)

val parse :
  source_name:string ->
  Token.positioned list ->
  (Ast.file, Load_error.t) result
(** [parse ~source_name tokens] consumes the token stream (terminated by
    [Token.EOF]) and returns the concrete syntax tree.

    [source_name] is propagated as [file] on [Ast.file.source_name] and is
    used only for error messages; it is the caller's responsibility that
    it matches the [~file] passed to [Lexer.tokenize].

    Reports the first parse error encountered. Follow-up errors after a
    recovery point are deferred to v1. *)
