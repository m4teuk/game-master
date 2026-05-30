# `.game` Language Specification (v0) — Overview

This is the top-level specification for the `.game` domain-specific language. It describes design principles, the overall model, and required file contents. For implementation-level detail, see the companion docs.

**Companion documents (all required for a complete implementation):**

- [grammar.md](./grammar.md) — lexical rules and concrete grammar (parser reference)
- [type-system.md](./type-system.md) — types, inference, equality, exhaustiveness (type checker reference)
- [runtime.md](./runtime.md) — engine contract, view computation, rng, action log (runtime reference)
- [stdlib.md](./stdlib.md) — complete list of provided functions (reference)

---

## 1. Design principles

1. **Everything is typed.** All state shapes, action types, and result types are declared up front. No untyped escape hatches.
2. **Hidden information lives in piles, never in config.** Configs are always public. Anything secret — a hand, a face-down deck, a hidden bid, a hidden team assignment — is a pile with appropriate visibility.
3. **Validation operates on views, not state.** A rule cannot accidentally reference hidden information when checking move legality, because it does not have access to it.
4. **`apply` is total.** Given a validated action, it produces a new state or it crashes via `fatal` for an engine-detected impossibility. No runtime error paths.
5. **Pure functional.** State is data; rules are code; rng is threaded separately; replay is `setup + action log`.
6. **No booleans.** `Flag = Off | On` is the one built-in two-state type, for the pervasive settled/not-settled case. For richer choices, users declare algebraic types — which preserve information better than `bool`.
7. **No premature optimization.** Played by humans; no caching, no special-casing until proven necessary.

---

## 2. The model

A `.game` file declares:

- **Types** for cards, actions, game outcome, global config, per-player config, and any auxiliaries.
- **Piles** — named, typed, ordered collections of cards, each with a visibility function. The only place hidden information can live.
- **Options** — a declarative form the engine renders as CLI / TUI / JSON for table setup.
- **Functions** — five required (`setup`, `validate`, `apply`, `terminal`, plus per-pile `visibility`) and any helpers.

The engine drives the game by calling the required functions in a fixed loop; see [runtime.md §3.2](./runtime.md).

### 2.1 State and view

`State` is the game position at a moment in time. `View` is what one player sees: the same shape as state, but with pile contents replaced by `PileView` values (`Contents(list)`, `Size(n)`, or `Masked`) depending on each pile's visibility function.

Both are **opaque** to rulesets — they are manipulated only through stdlib helpers. This keeps the engine's internal representation free to evolve without breaking games.

The **player roster** (who is playing) is part of both `State` and `View`, accessible via stdlib `players_of(state)` and `players_of_view(view)`. The roster is fixed at setup and does not change during a game. Anything mutable about players (turn order, eliminations, teams, scores) is the ruleset's responsibility and lives in `Config` or `PlayerDict`.

### 2.2 Card movement invariant

Once `setup` returns, every card lives in exactly one pile at all times, and the only way for a card to change location is a pile-to-pile move. There is no `add_card` or `remove_card`. `apply` may use `temp_pile()` for scratch space, which the engine garbage-collects after each call.

Consequences: the action log fully describes physical motion; replay and animation work without auxiliary events; "where did this card come from?" always has an answer.

### 2.3 RNG

The random number generator is **not** part of `State`. It is a separate value passed alongside. `setup` and `apply` receive `rng: RNG` as a parameter; authors consume it via stdlib calls (`shuffle`, `random_int`, …). The engine tracks advancement automatically. This prevents a class of "forgot to update the rng" bugs.

See [runtime.md §6](./runtime.md) for the precise model.

---

## 3. Required file contents

Every `.game` file must declare these five types:

| Name | Role |
|------|------|
| `Card` | Sum of all card kinds in this game |
| `Action` | Sum of all player moves |
| `Outcome` | Game result (the `R` in `GameStatus<R>`) |
| `Config` | Single-constructor record: global public state schema |
| `PlayerDict` | Single-constructor record: per-player public state schema (may be empty) |

And these functions:

**Core game logic:**

```
fn setup(players: List<PlayerId>, options: Options, rng: RNG)
  -> Result<State, Text>

fn validate(view: View, player: PlayerId, action: Action)
  -> Result<Unit, Text>

fn apply(state: State, rng: RNG, action: Action, player: PlayerId)
  -> State

fn terminal(state: State) -> GameStatus<Outcome>
```

**Text I/O** (for CLI and log rendering; see §3.1):

```
fn action_to_text(action: Action, player: PlayerId) -> Text
fn text_to_action(input: Text, view: View, player: PlayerId)
  -> Result<Action, Text>
fn view_to_text(view: View, player: PlayerId) -> Text
fn outcome_to_text(outcome: Outcome, player: PlayerId) -> Text
```

Plus per-pile `visibility` functions declared inline with each pile.

Details:

- **`setup`** constructs initial state. Consumes rng via stdlib. Returns `Err(msg)` for invalid player count or bad options. Engine tracks rng advancement.
- **`validate`** checks an action's legality from the acting player's view. Returns `Err(msg)` for user-facing errors. Has no access to full state — information leakage is impossible by construction.
- **`apply`** is total. Assumes the action was validated. Produces a new state. Consumes rng via stdlib.
- **`terminal`** is polled after every successful `apply`. Returns `Ended(outcome)` to end the game.

### 3.1 Text I/O functions

These render game values for human consumption and parse player input. They are required — every game declares all four — but a one-line delegation to stdlib built-ins (`builtin_action_to_text`, `builtin_text_to_action`, `builtin_view_to_text`, `builtin_outcome_to_text`) is the common implementation. Custom implementations give authors full control over CLI formatting.

- **`action_to_text(action, player)`**: renders an action for log and history display. Must be self-describing (it is read with no surrounding context) and does not receive a `View` — intentional, to keep log entries universally readable.
- **`text_to_action(input, view, player)`**: parses a player's raw text into an `Action`. Receives the view for context-sensitive parsing ("play this" → which card). Returns `Err(msg)` on parse failure; the engine shows the error. **Does not validate game logic** — a parsed-but-illegal action is caught by `validate` downstream. Information leakage is not a concern since the parser operates on the same view the player sees.
- **`view_to_text(view, player)`**: renders the game state as the player sees it, for CLI display.
- **`outcome_to_text(outcome, player)`**: renders the final outcome, personalized for the player ("You won!" vs. "Alice won").

### 3.2 Options

`Options` is synthesized by the engine from the `options { … }` block; if no block is present, `Options = Unit`. See [type-system.md §1.2](./type-system.md).

---

## 4. Engine contract (summary)

Fixed per-action loop (full detail in [runtime.md §3](./runtime.md)):

1. Receive action `a` from player `P`.
2. Compute `v = view(state, P)`.
3. If `validate(v, P, a)` returns `Err(msg)`, reject with `msg`; continue.
4. `state' = apply(state, rng, a, P)`; engine advances rng.
5. Append `(P, a)` to the action log.
6. If `terminal(state')` is `Ended(o)`, end the game with `o`.
7. Else broadcast new views to all players and continue.

---

## 5. Minimal skeleton

```
-- types
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card { suit: Suit, rank: Num }
type Action = PlayCard { card: Card } | Pass
type Outcome = Winner { player: PlayerId }
type Config = Config { turn: PlayerId, top: Card }
type PlayerDict = PD {}

-- piles
pile Deck of Card visibility = public_size
pile Hand(owner: PlayerId) of Card visibility = owner_only(owner)
pile Discard of Card visibility = public

-- options
options { starting_hand: Num = 7 }

-- functions
fn setup(players: List<PlayerId>, options: Options, rng: RNG)
  -> Result<State, Text> = ...

fn validate(view: View, player: PlayerId, action: Action)
  -> Result<Unit, Text> = ...

fn apply(state: State, rng: RNG, action: Action, player: PlayerId)
  -> State = ...

fn terminal(state: State) -> GameStatus<Outcome> = ...
```

For a complete worked example, see `war.game`, `go_fish.game`, or `crazy_eights.game`.

---

## 6. Open items deferred from v0

- **Numeric type split.** One `Num` or separate `Int`/`Float`? v0 uses one.
- **Pretty-printing / display hints** for cards and actions in CLI output. The engine currently formats via the action's constructor name + fields generically; custom formatters are future work.
- **Module / import system.** Not needed for v0 — games fit in one file.
- **Option field constraints** (the dropped `where` clause). Currently fields only have types and defaults; range restrictions live inside `setup`.
- **Mixed-visibility piles.** Forbidden in v0; split into multiple piles. Future work could add a per-card visibility function.
- **Game-master / hidden-info-driven rules.** The v0 workaround is "player submits action, GM verifies as a separate action." Not a language feature.
- **Spectators, undo/takeback, in-game chat.** All parkable; the model accommodates them without changes.
- **Multi-round matches.** The engine handles one round. Match orchestration (Hearts to 100, Bridge rubbers) is a layer above.
