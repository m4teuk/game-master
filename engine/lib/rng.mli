(** Deterministic 128-bit-seeded PRNG (runtime.md §6).

    Functional interface: every consumer receives a new [t]. The engine
    threads the current [t] through [setup] and [apply] and does not
    expose it to the ruleset (rulesets only interact through stdlib). *)

type t

val of_seed : bytes -> t
(** [of_seed seed] must be exactly 16 bytes (128 bits). Any other length
    raises [Invalid_argument]. Two calls with equal seeds produce PRNGs
    that emit identical sequences. *)

val random_int : t -> lo:int -> hi:int -> int * t
(** Uniform over the inclusive integer range [[lo, hi]]. Raises
    [Invalid_argument] if [lo > hi]; callers convert to [fatal] when the
    condition originates in user code. *)

val shuffle_list : t -> 'a list -> 'a list * t
(** Fisher-Yates. The element type is parametric since stdlib uses it for
    both [List<T>] and [List<Card>]. *)
