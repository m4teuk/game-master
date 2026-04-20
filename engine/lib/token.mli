(** Lexical tokens produced by [Lexer].

    Token shapes mirror grammar.md §1. Every positioned token carries a
    source span so downstream stages can attribute errors. *)

type t =
  (* identifiers and literals *)
  | TYPE_IDENT of string       (** [A-Z][A-Za-z0-9_]* *)
  | VALUE_IDENT of string      (** [a-z_][A-Za-z0-9_]* — including the wildcard [_] *)
  | NUM_LIT of int
  | TEXT_LIT of string

  (* keywords (grammar.md §1.4) *)
  | KW_FN
  | KW_LET
  | KW_IN
  | KW_MATCH
  | KW_TYPE
  | KW_PILE
  | KW_OF
  | KW_VISIBILITY
  | KW_MOD

  (* punctuation (grammar.md §1.7) *)
  | LPAREN   (** [(] *)
  | RPAREN   (** [)] *)
  | LBRACE   (** [{] *)
  | RBRACE   (** [}] *)
  | LBRACK   (** [[] *)
  | RBRACK   (** [\]] *)
  | COMMA
  | COLON
  | SEMI
  | EQ
  | ARROW    (** [->] *)
  | PIPE     (** [|] *)
  | DOTDOT   (** [..] *)
  | LT       (** [<] *)
  | GT       (** [>] *)
  | PLUS
  | MINUS
  | STAR
  | SLASH

  (* reserved but unused in v0 — the parser rejects with a specific message *)
  | PIPE_FORWARD (** [|>] *)

  | EOF

type positioned = {
  token : t;
  span : Load_error.span;
}

val to_string : t -> string
(** Human-readable rendering, used in parse error messages
    (e.g. ["expected ARROW, got 'fn'"]). *)
