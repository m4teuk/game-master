# Editing the stdlib

Touch-points adding, removing, or changing a stdlib function.
Just a checklist so nothing is forgotten.

## Where things live

| Concern | File |
|---|---|
| Spec (user-facing signatures) | `../../docs/dot-game-dsl/stdlib.md` |
| Signatures seeded into the type env | `lib/builtins.ml` — `seed_values` |
| Runtime implementations | `lib/stdlib_impl.ml` — one `impl_<name>` per fn, then an entry in `all` |
| Runtime value constructors (if adding one) | `lib/value.ml` / `.mli` |
| Rendering (text + JSON) | `lib/render.ml` |
| Typechecker helpers (ctor templates, admissibility) | `lib/typecheck/tc_expr.ml`, `lib/types.ml` |
| Example games that might break | `../../game-examples/*.game` |

## Adding a stdlib function

Five edits, same order every time:

1. **Spec** — add the signature and one paragraph of semantics to `stdlib.md`, plus a row in the §16 availability matrix.

2. **Seed the signature** in `builtins.ml::seed_values`. Use named params so keyword-arg calls resolve:
   ```
   |> add_fn "my_fn" [("x", Types.T_num); ("y", Types.T_num)] Types.T_num
   ```
   Convenience helpers `v`, `list`, `result`, `fn`, `user`, `tuple`, `pile_ref`,
   `pile_view` are defined at the top of the file.

3. **Write the implementation** in `stdlib_impl.ml`. Convention: `impl_<name> ctx args`, pattern-match on the expected arg shape, raise `Value.Fatal` on anything else via the `type_err` helper. If the fn doesn't use `ctx`, prefix it with `_`.
   ```
   let impl_my_fn (_ : ctx) args =
     match args with
     | [V_num x; V_num y] -> V_num (x + y)
     | _ -> type_err "my_fn" args
   ```

4. **Register** in the `all` table at the bottom of `stdlib_impl.ml`:
   ```
   { name = "my_fn"; capabilities = []; impl = impl_my_fn };
   ```
   Capabilities must match what the spec's §16 availability says. Predefined groups:
   - `state_caps` = setup/apply/terminal/visibility (anything that has `State`)
   - `view_caps` = validate/text_to_action/view_to_text (anything that has `View`)
   - `setup_apply_caps` = setup/apply (has `State` and `RNG`)
   - Empty list `[]` means universal.

5. **Verify**:
   ```
   dune build
   dune exec bin/cli.exe -- check ../game-examples/war.game
   ```
   Optionally write a tiny test .game file that exercises the new fn and run it through `session`.

## Removing a stdlib function

Reverse of the add list:

1. Remove the `impl_*` function and the `all` table entry in `stdlib_impl.ml`.
2. Remove the `add_fn` line from `builtins.ml::seed_values`.
3. Update `stdlib.md` (drop the signature and the §16 row).
4. Grep `../../game-examples/` for uses — update or delete call sites.
5. `dune build && dune exec bin/cli.exe -- check ../../game-examples/*.game`.

## Changing a signature (rename, type change, arg reorder)

1. `stdlib.md` — describe the new shape.
2. `builtins.ml::seed_values` — update types and param names.
3. `stdlib_impl.ml` — update the `impl_*`'s pattern match. If args were reordered, the positional match must reflect the new order.
4. Example games — a signature change typically cascades; run `check` on all three and fix call sites that break.

Notes:
- Param names (the strings in `add_fn`) are what `keyword=value` calls match against. Renaming `("from", …)` to `("source", …)` breaks `move_top(s, from=…)` call sites.
- A function that returns another function should return `V_partial { arity; impl }` — see `impl_owner_only` for the pattern.

## Adding/changing ctors or types

Touch a builtin ADT (Visibility, Flag, PileView, etc.) or add a brand-new opaque — these are rarer but spider across more files.

**New ctor on an existing ADT**, e.g. a `Visibility::TeamView` variant:
1. `builtins.ml::seed_types` — add to the `add_adt` for that type.
2. `value.ml` — add a convenience constructor if useful (`let team_view = V_ctor { … }`).
3. `render.ml::json_to_value` — if the ADT is a generic builtin handled there (Result/GameStatus/PileView), add the new variant to its `choices` list. User ADTs go through `convert_user_ctor` and need no change.
4. `stdlib.md` — update the algebraic-built-ins list in §1 and any `Visibility` docs.

**New opaque type** (something like `RNG`/`State`/`View`):
1. `types.ml` / `.mli` — add a `T_<name>` arm. Extend `string_of_ty`, `equality_admissible` (usually `false` for opaque), `apply_subst` (no-op), `unify` (reflexive pair).
2. `tc_resolver.ml::builtin_nullary` — so `"MyOpaque"` in type expressions resolves to `T_my_opaque`.
3. `tc_pass_decls.ml::contains_opaque` — include it so user types can't embed it.
4. `value.ml` / `.mli` — add `V_my_opaque` (or a record). Update `equal` (raise if not admissible).
5. `render.ml::value_to_text` / `value_to_json` — add a sentinel like `<my_opaque>` or explicit handling.
6. `interp.ml` — if the value can be passed as a function argument (usually yes), handle it in `lookup_name` / `call` defaults. Usually nothing changes; dispatch is already by outer variant.
7. `stdlib.md` / `type-system.md §1.1` — document.

## Sanity-check before committing

```
dune build                                     # no warnings, no errors
for f in war crazy_eights go_fish; do
  dune exec bin/cli.exe -- check ../../game-examples/$f.game
done
```

If any stage regresses, the CLI's `pipeline`, `ast`, `tast`, or `toplevel` commands will show where.

## Design invariants to preserve

- **Capability gating**: every stdlib fn's `capabilities` list reflects which contexts have the required inputs in scope. Don't widen unless the spec's §16 matrix is updated too.
- **Field names in `V_ctor`**: `impl_ok` / `impl_err` / `Value.ok` / `Value.err` etc. build ctors with the exact field names the typechecker seeds (`"value"`, `"error"`, `"items"`, `"n"`, `"outcome"`). Pattern matches against these rely on the names being stable.
- **State mutations return a new `V_state`**: never mutate the record in place. `{ s with state_piles = … }` is the idiom.
- **RNG threading**: any stdlib fn that advances the RNG reads `ctx_rng ctx`, dereferences, calls the Rng primitive, and writes back with `:=`. Don't copy the ref.
- **Fatal surface**: `Value.Fatal` (aka `Interp.Fatal`) is the only exception that should ever leave a stdlib impl. The engine entry points catch it and `Stack_overflow`, convert to `Fatal _` in `apply_error` / `Setup_fatal _` etc.
