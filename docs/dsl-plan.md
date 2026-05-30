# `.game` DSL plan — concrete analysis + the v0.1 stdlib

This doc replaces the speculative parts of `dsl-gaps.md` with concrete
numbers, a concrete plan, and a concrete implementation of the non-breaking
subset of that plan.

## 1. Measured savings

Every existing `.game` file has two siblings that implement the same
rules against successive language versions. LOC is source lines
excluding blank lines and `--` comments.

- `*_new.game` — v0.1: pure stdlib additions (§16), optional text-I/O.
- `*_v2.game` — v0.2: adds `if-then-else`, comparison operators
  (`<`, `<=`, `==`, `!=`, `>=`, `>`), short-circuit `&&` / `||`, and
  auto-injected defaults for `Card` / `Suit` / `PlayerDict`.

| Game              | Orig | v0.1 (`_new`) | v0.2 (`_v2`) | Δ vs orig |
| ----------------- | ---: | ------------: | -----------: | --------- |
| `higher_or_lower` |  100 |            56 |           43 | −57%      |
| `pig`             |  119 |            52 |           50 | −58%      |
| `old_maid`        |  176 |            77 |           62 | −65%      |
| `war`             |  174 |            90 |           81 | −53%      |
| `blackjack`       |  196 |           113 |           95 | −52%      |
| `cheat`           |  200 |           103 |           93 | −54%      |
| `go_fish`         |  299 |           171 |          164 | −45%      |
| `crazy_eights`    |  326 |           177 |          166 | −49%      |

The v0.2 cut is substantially sharper for the shorter games. The two big
phase-machine games (`go_fish`, `crazy_eights`) get a smaller percentage
cut because their bulk is inherent case analysis across a 3-ctor phase
× 5-or-6-ctor action matrix, which no amount of stdlib sugar can collapse.

## 2. Shipping set (v0.1)

Everything in this section is a pure, non-breaking addition. All 8 original
games compile and run unchanged. New games opt in by calling the new names.

### 2.1 Stdlib growth (`stdlib.md §16`)

```
range, take, drop, count, find, concat, reverse, repeat, zip,
sum, is_empty, head, tail

gt, lt, gte, lte, eq_num, ne_num, between, min, max,
is_zero, is_positive, is_negative

flag_and, flag_or, flag_not, when_flag

require

fresh_deck, card_rank, card_suit, card_has_rank, card_has_suit,
cards_of_rank, cards_of_suit

hand_visibility

deal_evenly, deal_cycle

refill
```

### 2.2 Linker change: text-I/O functions are now optional

If a ruleset doesn't declare `action_to_text` / `text_to_action` /
`view_to_text` / `outcome_to_text`, the linker synthesizes a one-line
delegation to the matching `builtin_*_to_*`. Rulesets that still declare
them explicitly are unchanged.

### 2.3 What didn't ship, and why

- **`options_of(state)` / `view_options(view)`.** Would remove the
  need to stash option values in `Config`. Requires carrying options on
  every `State` and `View` value (or threading them through `Interp.ctx`).
  ~5 edits across 3 modules. Deferred because it's the smallest invasive
  thing and the current workaround (copy into Config) is only 1–2 extra
  fields per game.
- **Built-in `Card` / `Suit` types.** Would remove the two required
  type declarations for any game that uses the standard shape. Requires
  loosening the linker's "required user types" check. Deferred.
- **`if cond then a else b` / comparison operators.** Parser + grammar
  change. Biggest ergonomic win still on the table — `match compare(…)`
  and `match if_eq(…)` remain the dominant noise even in the `_new`
  files.
- **Range literal `[2..14]`.** Lexer + parser change; `range(2, 14)` is
  the stdlib substitute.

## 3. Where the remaining lines go

Take `higher_or_lower_new.game` (56 LOC). Its decomposition:

```
7   types               (Card, Suit, Action, Phase, Outcome, Config, PlayerDict)
3   pile declarations
1   options block
16  setup
1   validate (trivial)
22  apply
8   terminal
------------
58   (one line absorbed by formatting)
```

The 7 type declarations and 3 piles are load-bearing — the engine can't
infer them. Setup is long because of nested matches for options
destructuring + player-count checks. Apply is 22 lines largely because
every branch on numbers (`compare`) takes 3 lines and every branch on a
`Result` takes 5.

Across all 8 `_new` games, the floor is set by:

| Cost per game | Lines | Fixed by                              |
| ------------- | ----: | ------------------------------------- |
| 5 type decls  |  5–10 | built-in `Card`, `Suit`, `PlayerDict` |
| Pile decls    |   3–4 | n/a (truly game-specific)             |
| Setup ceremony|   10+ | `if-else`, options_of, range literal  |
| Validate gates|   5–20| `require` + `?`-style early return    |
| Apply match noise | 20–60 | `if-else`, comparison ops         |
| Terminal      |   5–10| `if-else`                             |

Everything after "Pile decls" in that table is a language-level change,
not a stdlib one. The realistic target with v0.1 as-is is:

- 40–60 LOC for simple games (higher_or_lower, pig, old_maid) — already
  achieved.
- 80–120 LOC for games with a phase machine (war, cheat, blackjack) — achieved.
- 150–180 LOC for games with rich phase machines + multi-book state
  (crazy_eights, go_fish) — achieved.

## 4. The next slice to hit 20–40 LOC

To actually bring even the complex games into the 20–40 LOC bracket, the
following would need to land (ordered by LOC impact per line-of-engine-code):

1. **`if cond then a else b` for `Flag`.** Eliminates most `match flag {
   On -> …; Off -> … }`. ~40 lines saved across all 8 games. Cost: ~80
   lines of parser/typechecker. Use `when_flag` as the semantic model —
   we already proved it composes.

2. **Comparison operators `<`, `<=`, `==`, `>=`, `>` for `Num`.**
   Every `match compare(n, k) { … }` becomes `if n == k then …`. Another
   ~60 lines saved, and it makes `if-else` pull its weight.

3. **Built-in `Card`, `Suit`.** Removes 2 type declarations per game;
   the 8-game corpus loses 16 lines and the `type Suit = Clubs | …`
   clutter goes with it. Cost: linker relaxation + stdlib type seeding.

4. **Optional `PlayerDict = PlayerDict {}`.** When no per-player state is
   needed, skip the declaration entirely. 1 line per game.

5. **`options_of(state)` / `view_options(view)`.** Saves 1–3 Config
   fields per game that has options. Cost: 5-edit engine change.

6. **`?` / `let?` for Result chains.** Replaces `and_then(r, fn (_u) -> …)`
   with a terser short-circuit form. Across the 8 games this appears
   30+ times.

Doing 1+2+3 plausibly gets the simple games into the 25–35 LOC range and
the complex ones to ~70–100.

## 5. What's in this commit

### Code
- `engine/lib/builtins.ml`: +78 lines of new stdlib signatures.
- `engine/lib/stdlib_impl.ml`: +279 lines of implementations + dispatch
  entries.
- `engine/lib/link.ml`: text-I/O auto-defaulting (40 new lines).
- `engine/lib/interp.ml` / `stdlib_impl.mli`: §17 reference updates.

### Docs
- `docs/dot-game-dsl/stdlib.md`: new `§16 Extended stdlib` section
  (§17 availability matrix unchanged except for the section number).
- This file.

### Games
- `game-examples/*_new.game`: one per existing game, proving the
  stdlib additions actually shorten real rulesets.

### Non-breaking guarantee
- No tokens added to the lexer.
- No grammar productions changed.
- No existing stdlib names overloaded or repurposed.
- Required-user-type checks unchanged.
- `apply` / `validate` / `setup` / `terminal` signatures unchanged.
- Every original `_` suffix-less game file is byte-identical.
