# `.game` Runtime Contract

This document specifies what the engine is responsible for: how it loads a `.game` file, drives the game loop, computes views, manages piles, tracks the RNG, and handles fatal conditions.

Companion docs: [grammar.md](./grammar.md), [type-system.md](./type-system.md), [stdlib.md](./stdlib.md).

---

## 1. Engine boundary

The engine is the authoritative host. A `.game` ruleset is pure data: types and functions. The engine:

- Owns the player roster.
- Owns the RNG state.
- Owns the action log.
- Owns the current `State`.
- Calls into ruleset functions (`setup`, `validate`, `apply`, `terminal`, and each pile's `visibility`) with controlled inputs and collects their outputs.
- Serves views to clients and receives actions from them.

The ruleset does not observe anything the engine does not pass it. In particular: the ruleset has no access to wall-clock time, the identity of clients beyond their `PlayerId`, network state, or the action log itself.

---

## 2. Values outside `State`

Four pieces of live data are held by the engine, not by `State`:

1. **Player roster**: `List<PlayerId>`, fixed at setup. Exposed to rulesets via `players_of(state)` and `players_of_view(view)` — the engine attaches it to every `State` and `View` it hands out. The roster is **immutable** across a game session: no joins mid-game, no roster changes on disconnect. Client dropouts are a transport concern, not a state concern.
2. **RNG**: current state of a deterministic PRNG.
3. **Action log**: `List<(PlayerId, Action)>`, append-only.
4. **Ruleset identifier**: a stable name + version of the loaded `.game` file.

`State` is exactly the game position as the ruleset sees it (config, piles, player dicts, plus the engine-attached roster). Nothing operational lives there. This makes state serialization, equality, and diffing well-defined.

---

## 3. Game lifecycle

### 3.1 Setup

Inputs to the engine at game start:

- A parsed, type-checked ruleset.
- A `List<PlayerId>` (the players, in seat order).
- An `Options` value, constructed from user input against the ruleset's `options { … }` declaration. If the ruleset has no `options` block, the engine supplies the singleton zero-field value `Options { }` (per type-system.md §1.2).
- A 128-bit seed (see §6).

Engine steps:

1. Initialize the RNG from the seed.
2. Call `setup(players, options, rng)`:
   - If the call returns `Ok(state)`: the initial state is `state`.
   - If it returns `Err(msg)`: game setup fails; report `msg` to the operator and abort.
3. Initialize the action log to empty.
4. Broadcast initial views: for each player `p`, compute `view(state, p)` and render it via `view_to_text(v, p)` for display.

The engine also records the seed, the ruleset ID + version, and the player roster alongside the log for replay.

### 3.2 Action loop

For each incoming text input `input` from player `P`:

1. Compute `v = view(state, P)`.
2. Parse: `text_to_action(input, v, P)`:
   - `Err(msg)` → reply to `P` with `msg`. Continue the loop.
   - `Ok(a)` → continue with action `a`.
3. Validate: `validate(v, P, a)`:
   - `Err(msg)` → reply to `P` with `msg`. No state change, no log entry. Continue the loop.
   - `Ok(Unit)` → continue.
4. Apply: `state' = apply(state, rng, a, P)` (see §6 for the rng side).
5. Append `(P, a)` to the action log.
6. For each player `q`, broadcast the new view rendered as text: `view_to_text(view(state', q), q)`. Also broadcast the action that just happened, rendered via `action_to_text(a, P)`, for history display.
7. Call `terminal(state')`:
   - `Ended(outcome)` → the game ends. For each player `q`, broadcast `outcome_to_text(outcome, q)`. Stop.
   - `Ongoing` → replace `state` with `state'`. Continue.

Clients that want raw (non-text) access can request the underlying values directly; the engine exposes both rendered and raw surfaces. The text I/O functions are the default and recommended path.

### 3.3 Termination

When the loop terminates via `Ended(outcome)`:

- Persist the tuple `(ruleset_id, version, seed, players, action_log, outcome)` as the replay record.
- Release per-game resources.
- Notify clients.

---

## 4. View computation

A view is the per-player projection of `State` that masks non-visible pile contents.

```
View = { config: Config, player_dicts: Map<PlayerId, PlayerDict>, piles: Map<PileKey, PileView<C>> }
```

To compute `view(state, P)`:

1. Copy `config` directly (always public).
2. Copy all `player_dicts` directly (always public).
3. For each materialized pile instance `(name, keys, contents)` (§5):
   - Call the pile's declared `visibility` function with `(state, P)`.
   - If result is `SeeAll`: emit `Contents(contents)`.
   - If result is `SeeSize`: emit `Size(length(contents))`.
   - If result is `Hidden`: emit `Masked`.
4. Return the view.

Visibility functions are called by the engine, pure, and must not fatal or return `Err` in normal operation. A `fatal` from a visibility function is an engine bug — the ruleset's `visibility` should be total.

---

## 5. Pile registry

### 5.1 Pile instances

A pile *declaration* (`pile Name(k1: T1, …) of C`) does not by itself create a pile. A pile *instance* is a specific tuple `(Name, (k1, …, kn))` that has received cards via `init_pile` or a `move_*` call.

The engine maintains a registry of pile instances. An instance enters the registry the first time it appears as the `to` argument of a move or the target of `init_pile`. An instance with zero cards still exists in the registry once it has been materialized; subsequent moves to and from it are all legal.

A `PileRef<C>` that has never been materialized refers to an implicitly-empty pile. Calls like `cards_in(state, Hand(X))` on a never-touched `Hand(X)` return `[]`. Calls that read the "top" return `Err` as for any empty pile.

### 5.2 Key types

Pile key types (the parameters of a parameterized pile, e.g. `owner: PlayerId, rank: Num`) must be equality-admissible ([type-system.md §7](./type-system.md)). The engine uses them as map keys. In practice this means: primitives (`Num`, `Text`, `PlayerId`) and user-declared enum-style or record types with equality-admissible fields.

### 5.3 View enumeration

When computing a view, the engine iterates over **materialized** pile instances only. An unmaterialized `Hand(NonExistentPlayer)` does not appear in the view. This avoids enumerating infinite key spaces (e.g. `Book(p: PlayerId, r: Num)` with arbitrary `Num`).

### 5.4 Temp piles

`temp_pile()` (available only inside `apply`) returns a fresh `PileRef<C>` whose identity is scoped to the enclosing `apply` call. Temp piles:

- Do not persist in `State` across `apply` calls.
- Exist in a per-call registry that is discarded after `apply` returns.
- Must be **empty at the time `apply` returns**. The engine asserts this and calls `fatal` if violated.
- Cannot be returned or stored in user types (they would escape their scope); the type system does not enforce this in v0, but doing so produces undefined behavior — treat it as an engine bug.

---

## 6. RNG

### 6.1 Algorithm

The engine uses a deterministic PRNG. The exact algorithm is implementation-defined but must satisfy:

- Seeded from a 128-bit seed.
- Deterministic: same seed + same call sequence → same outputs.
- Stable across engine minor versions for the same ruleset version. A version bump that changes RNG output constitutes a breaking change to replay compatibility.

Suggested implementations: ChaCha20 with a counter, PCG-XSL-RR-128.

### 6.2 Threading

The ruleset receives `rng: RNG` as a parameter to `setup` and `apply`. RNG is opaque — the ruleset can only pass it to stdlib functions that consume it (`shuffle`, `shuffle_list`, `random_int`).

The RNG value does **not** flow through return types. `setup` returns `Result<State, Text>`, `apply` returns `State`. The engine tracks RNG advancement independently:

- Before calling `setup` or `apply`, the engine captures the current RNG.
- RNG-consuming stdlib functions run in a mode where each consumption increments a call counter on the engine-held RNG.
- On return from `setup` / `apply`, the engine has the post-call RNG state ready for the next step.

Equivalently: the RNG passed to the ruleset is a *handle* into the engine's RNG, not an independent value. Multiple calls to RNG-consuming stdlib functions within one `apply` produce distinct outputs, as expected.

### 6.3 Purity implication

Because the RNG advances through stdlib calls, `setup` and `apply` are not pure in the strict mathematical sense — but they are pure relative to `(state, rng_state, inputs)`. Given the same triple, the same result. This is sufficient for replay.

---

## 7. Action log

### 7.1 Format

The action log is a list of entries. Each entry contains:

- `player_id`: the acting `PlayerId`.
- `action`: a serialized `Action` value.

Entries are appended in the order they were applied.

Serialization: structural, derived from the ruleset's declared `Action` type. A canonical form uses tagged JSON:

```
{ "tag": "PlayCard", "card": { "tag": "Card", "suit": "Clubs", "rank": 7 } }
{ "tag": "Pass" }
```

The exact wire format is engine-implementation-defined; what matters is that it is invertible — the engine can reconstruct the `Action` value from the log entry.

### 7.2 Scope

The log records **validated** actions only. Rejected actions are not logged. Actions carry no hidden information (see the card movement invariant), so the log can be broadcast to all players without leakage.

### 7.3 Replay

To replay: load the ruleset, initialize the RNG with the recorded seed, call `setup(players, options, rng)`, then fold over the action log applying each entry via `apply`. Any intermediate state is reproducible.

The engine must:

- Reject replay if the recorded ruleset ID + version does not match the loaded ruleset.
- Reject replay if the stored seed + player list + options don't match (to avoid confusion; the replay wouldn't actually be reproducible otherwise).

---

## 8. Errors and `fatal`

### 8.1 Three distinct error surfaces

1. **`Err(msg)` from `validate`.** A user-level "that move is illegal" message. The engine rejects the action, sends `msg` to the acting client, and continues the loop. Not logged to the action log.
2. **`Err(msg)` from `setup`.** A configuration error: wrong player count, bad options. The engine aborts game setup and surfaces `msg` to the operator. No game starts.
3. **`fatal(msg)`.** An engine-detected impossibility. The engine:
   - Captures a crash dump: `ruleset_id`, `version`, `seed`, `players`, `options`, `action_log`, current `state` snapshot, `msg`, the call stack if available.
   - Broadcasts a game-aborted message to clients (with `msg`).
   - Stops the game. Does not log the crash-causing call as a committed action.

### 8.2 `apply` is total

`apply` is required not to return `Err` or hit undefined behavior on validated input. If it does (via `fatal` or a runtime crash), it is an engine or ruleset bug.

The engine must not retry `apply`. The crash dump is the reproduction artifact; the game cannot proceed.

### 8.3 Visibility functions

Pile visibility functions should not `fatal` in normal operation. If one does, the engine treats it as a fatal crash with the same dump procedure.

---

## 9. Concurrency and ordering

The engine processes actions **one at a time**. There is no concurrent `apply`. Multiple clients may submit actions simultaneously over the network; the engine serializes them into a single stream and processes them in arrival order.

For simultaneous-action games (sealed bids), the ruleset models them as: each player submits to a hidden pile independently (each submission is one action), then a resolve step processes them all. The engine's serialization is orthogonal to this.

---

## 10. Size and resource limits

Recommended limits (implementation-defined in v0):

- Action log size: unlimited, but engines may cap per-session at some large value (e.g. 10⁶ actions).
- Pile instance count: unlimited in principle, bounded in practice by the game (with 6 players, `Book(p, r)` has 6 × 13 = 78 potential instances).
- Recursion depth: ruleset recursion depth is bounded by the host stack; the engine should handle `StackOverflow` as a fatal.
- Text message length: no hard cap; engines may truncate to a few KB when forwarding to clients.

These are not part of the language semantics; they are operational guidance.

---

## 11. Engine load sequence (summary)

When a ruleset is loaded:

1. **Lex and parse** the `.game` source ([grammar.md](./grammar.md)). Reject on lex/parse errors.
2. **Type-check** the declarations ([type-system.md](./type-system.md)). Reject on type errors.
3. **Link** built-ins: verify every required type (`Card`, `Action`, `Outcome`, `Config`, `PlayerDict`) and every required function (`setup`, `validate`, `apply`, `terminal`, `action_to_text`, `text_to_action`, `view_to_text`, `outcome_to_text`) is present with the correct signature. Reject if not.
4. **Synthesize** the `Options` type from the `options { … }` block (or alias to `Unit`).
5. **Register** the ruleset as ready.

Game loading happens once per ruleset version; one loaded ruleset may host many game sessions.
