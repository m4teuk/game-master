(** Seeds [Types.env] with the engine's built-in identifiers.

    Called once by [Typecheck.check] before any user declarations are
    inserted, so user names can be checked for collision against the
    built-ins via the regular [lookup_*] paths.

    This module owns the mapping from spec ([type-system.md §1.3],
    [stdlib.md]) to the [Types.env] representation. Anything callable or
    matchable from a user [.game] file but not declared by the user
    originates here. *)

val seed_types : Types.env -> Types.env
(** Adds the built-in algebraic types and their constructors:

    - Non-generic: [Visibility] (SeeAll, SeeSize, Hidden), [Ordering]
      (LT, EQ, GT), [Flag] (On, Off).
    - Generic: [Result<T,E>] (Ok, Err), [GameStatus<R>] (Ongoing, Ended),
      [PileView<C>] (Contents, Size, Masked).

    Generic constructors' field types reference their owner's type
    parameters via [T_var i], indexed positionally in declaration order
    (e.g. for [Result<T,E>], [T = T_var 0], [E = T_var 1]). *)

val seed_values : Types.env -> Types.env
(** Adds the stdlib functions ([stdlib.md] §1–§15) to [env.values] as
    [T_fn] types.

    Polymorphism uses [T_var i] indexed per-function (each signature's
    type variables are independent and resolved by
    [Types.unify_instantiation] at each call site).

    References to required user types ([Action], [Outcome], [Config],
    [PlayerDict], [Team]) appear as [T_user "<name>"] — forward
    references resolved when a call site is typechecked, not at seed
    time. Rulesets that omit a required type only fail typechecking if
    they actually call a stdlib function that mentions it.

    Per-callsite availability ([stdlib.md] §16: e.g. [temp_pile] is
    apply-only) is {b not} enforced here — that is a separate later
    pass over function bodies. *)
