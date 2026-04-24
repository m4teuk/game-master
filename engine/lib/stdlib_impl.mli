(** Implementations of every built-in function declared in stdlib.md.

    The dispatch-table entry type lives in [Interp] ([Interp.builtin]) to
    keep the two modules from depending on each other: [Interp] consumes
    the table via [make_ctx ~builtins]; this module only produces it.

    Capability checks (stdlib §17 availability matrix) are enforced here,
    not in [Interp], so the list of "which contexts may call [temp_pile]"
    lives next to the implementation it guards. *)

val all : Interp.builtin list
(** The complete table, passed to [Interp.make_ctx ~builtins] at context
    construction. *)

val lookup : string -> Interp.builtin option
