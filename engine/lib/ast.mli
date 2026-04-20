(** Concrete AST, emitted by [Parser].

    One-to-one with grammar.md §2. No resolution, no types — that is
    [Typecheck]'s job. Every node carries a source span so the type
    checker can pinpoint errors. *)

type span = Load_error.span

(** {1 Type expressions (grammar §2.7)} *)

type type_expr =
  | TE_app of string * type_expr list * span
    (** [TYPE_IDENT] optionally with type args: [List<T>], [Result<T, E>]. *)
  | TE_tuple of type_expr list * span
    (** [(A, B, C)] — always arity >= 2. *)
  | TE_fn of type_expr list * type_expr * span
    (** [(A, B) -> R]. *)
  | TE_var of string * span
    (** Bare [VALUE_IDENT] in a type-expr position. Only legal inside stdlib
        internal signatures; in user code, [Typecheck] raises an error. *)

(** {1 Patterns (grammar §2.8)} *)

type pattern =
  | P_wild of span                                 (** [_] *)
  | P_var of string * span                         (** [VALUE_IDENT] *)
  | P_num of int * span                            (** [NUM_LIT] *)
  | P_ctor of {
      name : string;
      fields : field_pat list option;
        (** [None] for nullary (e.g. [Pass]);
            [Some fs] for field-shaped (possibly empty: [Foo {}]). *)
      has_rest : bool;
        (** [true] if the pattern ends in [..] to ignore remaining fields. *)
      span : span;
    }
  | P_ctor_pos of {
      name : string;
      args : pattern list;
        (** Positional sub-patterns, in source order. The type checker
            resolves them against the constructor's declared fields in
            declaration order; arity must match exactly. Used primarily
            for built-in single-field constructors like [Ok(c)],
            [Err(_)], [Contents([t, ..])], but the form is permitted
            for any constructor whose arity matches. *)
      span : span;
    }
  | P_tuple of pattern list * span                 (** [(p1, p2, ...)] *)
  | P_list_exact of pattern list * span            (** [[]] or [[p1, ..., pn]] *)
  | P_list_cons of {
      heads : pattern list;
      rest : string option;
        (** [None] for anonymous [..], [Some x] for [..x]. *)
      span : span;
    }
    (** [[p1, ..., pn, ..rest?]]. *)

and field_pat = {
  field : string;
  sub : pattern option;
    (** [None] means field-punning: [Card { rank }] short for
        [Card { rank: rank }]. *)
  span : span;
}

(** {1 Expressions (grammar §2.9)} *)

type expr =
  | E_num of int * span
  | E_text of string * span
  | E_var of string * span                         (** [VALUE_IDENT] *)
  | E_ctor of string * span                        (** nullary [TYPE_IDENT] *)
  | E_record of {
      ctor : string;
      body : record_body;
      span : span;
    }
    (** Construction or update, per [record_body]. *)
  | E_tuple of expr list * span
  | E_list of expr list * span
  | E_paren of expr * span                         (** preserved for round-trip *)
  | E_app of expr * arg list * span                (** function call *)
  | E_let of {
      pat : pattern;
        (** LHS pattern. Must be irrefutable (see type-system.md §6.5). *)
      value : expr;
      body : expr;
      span : span;
    }
  | E_match of {
      scrutinee : expr;
      arms : (pattern * expr) list;
      span : span;
    }
  | E_lambda of {
      params : param list;
      body : expr;
      span : span;
    }
  | E_bin of bin_op * expr * expr * span
  | E_neg of expr * span                           (** unary [-] *)

and record_body = {
  spread : expr option;
    (** Update form: [Config { ..cfg, turn: p }]. When present, must appear
        first syntactically (grammar §2.9). [None] is construction. *)
  fields : (string * expr) list;
    (** Each entry is one [field_init]: ["field", expr] for
        [field: expr] and ["field", E_var ("field", _)] for punning. *)
}

and arg =
  | A_pos of expr
  | A_kw of string * expr                          (** [name = expr] *)

and param = {
  name : string;
  annot : type_expr option;                        (** optional, see type-system §5 *)
  span : span;
}

and bin_op =
  | Add       (** [+] *)
  | Sub       (** [-] *)
  | Mul       (** [*] *)
  | Div       (** [/] *)
  | Mod       (** [mod] *)

(** {1 Top-level declarations (grammar §2.1–§2.6)} *)

type field_decl = {
  name : string;
  ty : type_expr;
  span : span;
}

type ctor_decl = {
  name : string;
  fields : field_decl list option;
    (** [None] for nullary constructors.
        [Some []] is legal (empty-record constructor, e.g. [PD {}]). *)
  span : span;
}

type option_field = {
  name : string;
  ty : type_expr;
  default : expr;                                  (** required, grammar §2.4 *)
  span : span;
}

type top_decl =
  | D_type of {
      name : string;
      ctors : ctor_decl list;                      (** nonempty *)
      span : span;
    }
  | D_pile of {
      name : string;
      params : field_decl list;                    (** may be empty *)
      card_ty : type_expr;                         (** from [of <type>] *)
      visibility : expr;                           (** from [visibility = <expr>] *)
      span : span;
    }
  | D_options of {
      fields : option_field list;                  (** may be empty *)
      span : span;
    }
  | D_fn of {
      name : string;
      params : param list;
      ret : type_expr;
      body : expr;
      span : span;
    }
  | D_let of {
      pat : pattern;
        (** LHS pattern. Must be irrefutable (see type-system.md §6.5). *)
      body : expr;
      span : span;
    }

type file = {
  source_name : string;
  decls : top_decl list;                           (** order-independent *)
}

(** {1 Helpers} *)

val span_of_expr : expr -> span
val span_of_pattern : pattern -> span
val span_of_type_expr : type_expr -> span
val span_of_top_decl : top_decl -> span
