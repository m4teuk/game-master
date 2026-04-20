(** Internal representation of types and the type environment.

    Kept separate from [Typecheck] so that [Link], [Render], and
    [Interp] can consult the resolved environment without dragging the
    checker's internal traversal machinery with them. *)

(** {1 Types} *)

type ty =
  (* non-generic built-ins (type-system §1) *)
  | T_num
  | T_text
  | T_player_id
  | T_unit
  | T_rng
  | T_state
  | T_view
  | T_options

  (* generic built-ins *)
  | T_list of ty
  | T_result of ty * ty
  | T_game_status of ty
  | T_pile_view of ty
  | T_pile_ref of ty

  (* structural / user *)
  | T_tuple of ty list                    (** arity >= 2 *)
  | T_fn of ty list * ty
  | T_user of string                      (** nominal: name of a [type] or pile *)

  (* stdlib-internal only; user code triggers a type error if this appears *)
  | T_var of int                          (** De Bruijn index in a stdlib signature *)

(** {1 Constructor and type metadata} *)

type ctor_info = {
  ctor_name : string;
  owner_type : string;                    (** the type this constructor belongs to *)
  fields : (string * ty) list;            (** [] for nullary *)
  is_record : bool;
    (** [true] iff this is the sole constructor of its type AND its name
        equals the type name — the "record" case of type-system §3.1.
        Enables record-update syntax. *)
}

type type_info =
  | TI_builtin_opaque
    (** [State], [View], [RNG], [Options], [PileRef<_>]. No pattern matching,
        no user-side construction, no field access. *)
  | TI_adt of {
      ctors : ctor_info list;             (** at least one *)
      is_record : bool;                   (** convenience mirror of any ctor's [is_record] *)
    }

(** {1 Environment} *)

type env
(** Holds resolved type names, constructor names, value bindings, pile
    declarations, and the synthesized Options type. Abstract so it can be
    extended without churning downstream modules. *)

val empty : env

val lookup_type : env -> string -> type_info option
val lookup_ctor : env -> string -> ctor_info option
val lookup_value : env -> string -> ty option

val lookup_param_names : env -> string -> string list option
(** Returns the parameter names of a top-level fn or pile. Used to
    resolve keyword arguments at call sites. Returns [None] for
    stdlib values (whose params are unnamed at the user level). *)

(** {2 Extension}

    All [add_*] operations shadow silently on duplicate keys. Detecting
    duplicates (e.g. two [type] declarations of the same name) is the
    caller's responsibility — [Typecheck] performs that check before
    inserting, so it can attach a source span to the resulting error.

    Value-level shadowing is intentional: lambda parameters, let
    bindings, and pattern bindings legitimately shadow outer bindings
    of the same name (type-system §8.2). *)

val add_type : env -> string -> type_info -> env
val add_ctor : env -> string -> ctor_info -> env
val add_value : env -> string -> ty -> env
val add_param_names : env -> string -> string list -> env
(** Records parameter names for a value that supports keyword arguments
    (top-level fns and piles). Independent of [add_value] — call both. *)

val unify : (int * ty) list -> ty -> ty -> ((int * ty) list, string) result
(** Unify [a] and [b] against an existing substitution, extending the
    substitution. The caller threads [s] through multiple unifications.
    [T_user "Options"] and [T_options] unify successfully — they are
    the same nominal type, just two surface spellings. *)

val apply_subst : (int * ty) list -> ty -> ty
(** Substitute every [T_var i] in [t] with its binding in [s], chasing
    transitive bindings. Variables not in [s] are left alone. *)

val equality_admissible : env -> ty -> bool
(** Implements the recursive rule in type-system §7.1: equality-admissible
    iff primitive, built-in admissible ADT with admissible args, tuple or
    list of admissible, or user ADT whose ctor fields are all admissible.
    Requires [env] to resolve [T_user] nominally. *)

val unify_instantiation :
  params:ty list ->
  args:ty list ->
  return:ty ->
  (ty, string) result
(** For stdlib calls that take [T_var] parameters. Solves the substitution
    that makes each parameter match the corresponding argument; applies it
    to [return]. Error message describes the first conflict. *)

(** {1 Pretty-printing — for error messages only} *)

val string_of_ty : ty -> string
