(** Pile registry (runtime.md §5).

    A pile {i declaration} does not create a pile; a pile {i instance}
    [(name, keys)] enters the registry the first time it is the target of
    [init_pile] or a [move_*] call. Unmaterialized instances are
    implicitly empty.

    Temp piles (stdlib §5) live in a scoped sub-registry that is
    discarded when an [apply] call returns; see [Interp]. *)

type key = Value.t list
(** The argument tuple to a pile constructor, used as a map key. Must be
    equality-admissible (runtime.md §5.2) — enforced at type-check time,
    so the runtime trusts it. *)

type instance = {
  name : string;
  keys : key;
}

type t = Value.pile_entry list
(** Immutable registry, implemented as an association-style list of
    pile entries. Concrete type so [Value.state.state_piles] and other
    runtime modules can round-trip freely — an abstract type here
    would just force boilerplate converters. Every mutation returns a
    new [t]. *)

val empty : t

(** {1 Reads} *)

val cards_in : t -> instance -> Value.card list
(** Top-to-bottom order. Empty for unmaterialized instances. *)

val size_of : t -> instance -> int
val is_materialized : t -> instance -> bool

val materialized_instances : t -> instance list
(** For view enumeration (runtime.md §5.3): iterates {i only} over
    instances that have been materialized. Order is unspecified. *)

(** {1 Mutations}

    All operations that require a card present on an empty [from]
    raise [Interp.Fatal] with a descriptive message — callers surface
    that as a [fatal(...)] crash per runtime.md §8.1. *)

val init_pile : t -> instance -> Value.card list -> t
val move_top : t -> from_:instance -> to_:instance -> t
val move_card : t -> from_:instance -> to_:instance -> Value.card -> t
val move_to_bottom : t -> from_:instance -> to_:instance -> t
val move_all : t -> from_:instance -> to_:instance -> t
val move_all_to_bottom : t -> from_:instance -> to_:instance -> t

val shuffle : t -> Rng.t -> instance -> t * Rng.t

(** {1 Temp-pile scope} *)

type scope
(** A temp-pile sub-registry, owned by a single [apply] invocation. *)

val open_scope : unit -> scope
val fresh_temp : scope -> instance
(** Returns a fresh [instance] whose [name] is guaranteed not to collide
    with any user-declared pile name. The corresponding pile is
    unmaterialized until first use. *)

val close_scope : scope -> t -> (t, string) result
(** Asserts all temp piles were emptied before [apply] returned
    (runtime.md §5.4). On failure, the error message names the offending
    temp pile; callers raise [Interp.Fatal]. *)
