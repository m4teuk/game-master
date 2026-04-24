# `.game` Standard Library

Complete, committed list of functions the engine provides to `.game` rulesets. Signatures use the type-variable convention from [type-system.md §2](./type-system.md): uppercase single-letter names (`T`, `U`, `C`, `E`, `R`) and `Acc` are stdlib-internal type variables, universally quantified per call.

Companion docs: [grammar.md](./grammar.md), [type-system.md](./type-system.md), [runtime.md](./runtime.md).

---

## 1. Pile access (server-side, full info)

Called on `State`. Return full contents; no masking.

```
cards_in : (State, PileRef<C>) -> List<C>
```
Returns the pile's cards in order (index 0 is the top). Empty for unmaterialized or empty piles.

```
size_of : (State, PileRef<C>) -> Num
```
Number of cards in the pile. `0` for unmaterialized or empty piles.

```
top_of : (State, PileRef<C>) -> Result<C, Text>
```
Returns `Ok(card)` for the top card, `Err("pile is empty")` if the pile is empty or unmaterialized. Returning `Result` here (rather than a separate `is_empty` + unsafe top) composes cleanly with the rest of the error-handling idiom.

---

## 2. Pile access (view-side, masked)

Called on `View`. Return values masked per the pile's visibility function.

```
view_of : (View, PileRef<C>) -> PileView<C>
```
Returns `Contents(list)`, `Size(n)`, or `Masked` depending on visibility.

```
visible_size_of : (View, PileRef<C>) -> Result<Num, Text>
```
Returns `Ok(n)` for `Contents` or `Size` variants; `Err("size not visible")` for `Masked`.

```
visible_top_of : (View, PileRef<C>) -> Result<C, Text>
```
Returns `Ok(c)` if the pile's contents are visible and non-empty; `Err` if masked or empty (the message distinguishes: `"contents not visible"` vs. `"pile is empty"`).

---

## 3. Pile mutation

Return a new `State`. Callable from `setup` and `apply`.

```
move_top          : (State, from: PileRef<C>, to: PileRef<C>) -> State
move_card         : (State, from: PileRef<C>, to: PileRef<C>, card: C) -> State
move_to_bottom    : (State, from: PileRef<C>, to: PileRef<C>) -> State
move_all          : (State, from: PileRef<C>, to: PileRef<C>) -> State
move_all_to_bottom: (State, from: PileRef<C>, to: PileRef<C>) -> State
```

- `move_top`: moves the top of `from` to the top of `to`.
- `move_card`: finds `card` in `from` (by equality) and moves it to the top of `to`. If the card is not in `from`, calls `fatal`. If multiple cards are equal, moves the first-found (top-most).
- `move_to_bottom`: moves the top of `from` to the *bottom* of `to`.
- `move_all`: moves every card of `from` (in current top-to-bottom order) onto the top of `to`. `from` becomes empty.
- `move_all_to_bottom`: moves every card of `from` onto the bottom of `to`, preserving `from`'s top-to-bottom order.

All move operations on an empty `from` where the operation requires a card present (e.g. `move_top`, `move_to_bottom`, `move_card`) call `fatal`.

```
shuffle : (State, RNG, PileRef<C>) -> State
```
Shuffles the pile in place, consuming rng advancement. No-op on an empty or single-card pile.

There is deliberately no `add_card` or `remove_card`. See [runtime.md §2](./runtime.md) and the movement invariant in the main spec.

---

## 4. Setup-only

Available only inside `setup`. Using them from `apply` or any function called by `apply` is a runtime error caught by the engine.

```
new_state : (Config) -> State
```
Constructs an initial `State` from a `Config`. All piles are unmaterialized (implicitly empty). All player dicts are the default value for `PlayerDict` (see §13).

```
init_pile : (State, PileRef<C>, List<C>) -> State
```
Materializes the given pile with the given cards (index 0 becomes the top). If the pile already exists and is non-empty, appends to the top (equivalent to repeated `move_top` from a temp pile). In practice, authors use this once per pile during setup.

---

## 5. Apply-only

Available only inside `apply` and functions it calls.

```
temp_pile : () -> PileRef<C>
```
Returns a fresh ephemeral `PileRef<C>` scoped to the current `apply` call. The card type `C` is inferred from first use. The pile must be empty when `apply` returns.

---

## 6. Config, player dict, and roster

```
config_of         : (State) -> Config
view_config       : (View) -> Config
with_config       : (State, Config) -> State
player_dict       : (State, PlayerId) -> PlayerDict
update_player_dict: (State, PlayerId, (PlayerDict) -> PlayerDict) -> State

players_of        : (State) -> List<PlayerId>
players_of_view   : (View)  -> List<PlayerId>
```

`config_of` reads the config from state; `with_config` replaces it wholesale; record-update syntax (`Config { ..cfg, turn: p }`) handles field-level changes.

`player_dict(state, p)` returns `p`'s dict; if `p` is not in the player roster, calls `fatal`. All players start with the default `PlayerDict` (§13).

`update_player_dict(state, p, f)` applies `f` to `p`'s dict and stores the result.

`players_of` and `players_of_view` return the full player roster, in seat order. The roster is attached by the engine to every `State` and `View`; it is immutable for the game's lifetime ([runtime.md §2](./runtime.md)). Rulesets should use this instead of stashing a redundant copy in `Config`.

---

## 7. RNG

Available inside `setup` and `apply`. Consume and advance the rng via engine tracking.

```
random_int   : (RNG, lo: Num, hi: Num) -> Num
shuffle_list : (RNG, List<T>) -> List<T>
```

- `random_int(rng, lo, hi)`: returns a uniformly-distributed integer in `[lo, hi]` inclusive. `lo > hi` calls `fatal`.
- `shuffle_list(rng, xs)`: returns a fresh list with the same elements, randomly permuted.

See also `shuffle` (§3) for shuffling cards inside a pile.

---

## 8. Visibility helpers

Return `Visibility` values for use as (or inside) pile `visibility` functions.

```
public      : (State, PlayerId) -> Visibility   -- always SeeAll
public_size : (State, PlayerId) -> Visibility   -- always SeeSize
hidden      : (State, PlayerId) -> Visibility   -- always Hidden
owner_only  : (PlayerId) -> ((State, PlayerId) -> Visibility)
```

`owner_only(p)` returns a function: the resulting `(State, PlayerId) -> Visibility` yields `SeeAll` when the viewer equals `p`, else `Hidden`.

Team-based visibility (visible to all players on a given team) is not provided as a stdlib primitive in v0 — the notion of a team is ruleset-specific. Rulesets that need it write a custom inline visibility function, e.g. `visibility = fn (state, viewer) -> if_eq(team_of(state, viewer), our_team, SeeAll, Hidden)`.

---

## 9. Lists

All list operations are pure; they return new lists.

```
length   : (List<T>) -> Num
map      : (List<T>, (T) -> U) -> List<U>
filter   : (List<T>, (T) -> Flag) -> List<T>
fold     : (List<T>, Acc, (Acc, T) -> Acc) -> Acc
flatmap  : (List<T>, (T) -> List<U>) -> List<U>
append   : (List<T>, List<T>) -> List<T>
nth      : (List<T>, Num) -> Result<T, Text>           -- Err if out of range
member   : (List<T>, T) -> Flag                         -- requires T equality-admissible
any      : (List<T>, (T) -> Flag) -> Flag
all      : (List<T>, (T) -> Flag) -> Flag
split_at : (List<T>, Num) -> (List<T>, List<T>)         -- first N, then the rest
next_in_cycle : (List<T>, T) -> T                        -- requires T equality-admissible
```

Notes:

- `nth` is 0-indexed.
- `member` and `next_in_cycle` require the element type to be equality-admissible ([type-system.md §7](./type-system.md)). If not, it is a type error at the call site.
- `next_in_cycle(list, x)` returns the element after `x` in `list`, wrapping to `list`'s head after the last. Calls `fatal` if `x` is not in `list`.
- `split_at(list, n)` where `n > length(list)` returns `(list, [])`. Where `n < 0`, calls `fatal`.
- `fold` folds from left: `fold([a, b, c], z, f) = f(f(f(z, a), b), c)`.
- Predicates return `Flag` (not a built-in bool, which does not exist).

---

## 10. Result

```
ok       : (T) -> Result<T, E>
err      : (E) -> Result<T, E>
and_then : (Result<T, E>, (T) -> Result<U, E>) -> Result<U, E>
```

`ok(x)` is equivalent to writing `Ok(x)`; `err(e)` to `Err(e)`. They exist as functions (not just constructors) so they can be passed to higher-order functions.

`and_then` is monadic bind: `and_then(Ok(x), f) = f(x)`, `and_then(Err(e), f) = Err(e)`. Useful for chaining validation steps without deep match nesting.

---

## 11. Comparison and branching

```
compare : (Num, Num) -> Ordering
eq      : (T, T) -> Flag
if_eq   : (T, T, R, R) -> R
```

- `compare(a, b)`: defined only for `Num`. Returns `LT`, `EQ`, or `GT`.
- `eq(a, b)`: returns `On` iff `a == b`. Requires `T` equality-admissible. Equivalent to `if_eq(a, b, On, Off)` but lets you pattern-match the `Flag` directly.
- `if_eq(a, b, then, else)`: returns `then` if `a == b`, else `else`. Both branches are evaluated eagerly (no short-circuiting) — use `match` or `and_then` if you need lazy branching.

---

## 12. Fatal

```
fatal : (Text) -> a
```

Does not return. Aborts the game with the given message. The engine:

1. Captures a crash dump ([runtime.md §8.1](./runtime.md)).
2. Notifies clients.
3. Stops the game session.

The return type `a` is a free type variable, so `fatal` type-checks as any type an expression slot expects. Use for engine-detected impossibilities (e.g. "own hand should be visible," "can't advance an empty pile").

`fatal` is not for user-level errors. Validation failures return `Err`; setup failures return `Err` from `setup`.

---

## 13. Per-player dict defaults

`PlayerDict` has a single constructor with zero or more fields. When the engine creates a new game via `new_state`, each player receives a default `PlayerDict` constructed as follows:

- `Num` fields default to `0`.
- `Text` fields default to `""`.
- Fields of user-declared types must be explicitly initialized by the ruleset (the engine has no way to pick a default constructor for a multi-constructor type).

If a `PlayerDict` contains a field whose type has no sensible default, the ruleset must initialize all player dicts in `setup` via repeated `update_player_dict` calls. In practice, keeping `PlayerDict` to `Num` and `Text` fields avoids this entirely.

This rule also applies implicitly to non-`Num`, non-`Text` fields in `Config` during `new_state`, but since the ruleset explicitly constructs `Config` to pass to `new_state`, the problem doesn't arise there.

---

## 14. Text rendering built-ins

Default implementations of the four required text I/O functions. One-line delegation is the common case:

```
fn action_to_text(a, p)        -> Text = builtin_action_to_text(a, p)
fn text_to_action(t, v, p)     -> Result<Action, Text> = builtin_text_to_action(t, v, p)
fn view_to_text(v, p)          -> Text = builtin_view_to_text(v, p)
fn outcome_to_text(o, p)       -> Text = builtin_outcome_to_text(o, p)
```

Signatures:

```
builtin_action_to_text  : (Action, PlayerId)       -> Text
builtin_text_to_action  : (Text, View, PlayerId)   -> Result<Action, Text>
builtin_view_to_text    : (View, PlayerId)         -> Text
builtin_outcome_to_text : (Outcome, PlayerId)      -> Text
```

Behavior:

- **`builtin_action_to_text`** prints actions as `<player>: <ctor-form>`, where the ctor form is `Name` for nullary constructors or `Name(f1=v1, f2=v2, …)` otherwise. The acting player is prefixed so the broadcast form is self-contained (the log/transcript is readable without out-of-band context). Examples: `alice: Play(card=Card(suit=Clubs, rank=7))`, `bob: Pass`, `carol: AskFor(target="alice", rank=7)`. PlayerIds inside fields render via `player_id_to_text` as quoted text. Tuples use `(v1, v2, …)`, lists `[v1, v2, …]`.
- **`builtin_text_to_action`** parses the format `builtin_action_to_text` produces. The leading `<player>:` prefix is optional on input — the engine already receives the acting player as a parameter — so clients may submit either `alice: Play(…)` or just `Play(…)`. Whitespace between tokens is tolerated. On failure, the returned `Err` includes the parse position and a list of the ruleset's declared `Action` constructors so clients can show users what's accepted. Round-trip: `builtin_text_to_action(builtin_action_to_text(a, p), v, p) = Ok(a)` for any well-formed `a`.
- **`builtin_view_to_text`** prints a labeled block per pile, plus config and player dicts. Minimal formatting, meant as a fallback; authors who want a nicer layout write their own.
- **`builtin_outcome_to_text`** prints the outcome generically — no personalization. `Winner(player="alice")`, `Draw`, etc.

There is also a JSON variant for machine consumption:

```
builtin_action_to_json   : (Action, PlayerId)     -> Text
builtin_json_to_action   : (Text, View, PlayerId) -> Result<Action, Text>
builtin_view_to_json     : (View, PlayerId)       -> Text
builtin_outcome_to_json  : (Outcome, PlayerId)    -> Text
```

These emit canonical JSON (tagged union form), suitable for logs and network protocols. Authors who want "CLI is text, network is JSON" pick per context.

## 15. PlayerId conversion

```
player_id_to_text : (PlayerId)                      -> Text
text_to_player_id : (Text, View)                    -> Result<PlayerId, Text>
```

The engine stores player identities as whatever the transport provides (typically usernames). `player_id_to_text` returns that representation. `text_to_player_id(s, view)` looks up `s` against the roster (`players_of_view(view)`); returns `Err("no such player")` if not found.

These are provided as stdlib so rulesets don't have to re-invent player identity handling.

## 16. Extended stdlib (v0.1, non-breaking additions)

All additions in this section are pure stdlib — no parser, type-system, or
runtime-contract changes. Every existing game compiles unchanged. Games that
want terser rule code opt in by calling the new functions.

### 16.1 Extended list ops

```
range    : (Num, Num) -> List<Num>            -- inclusive on both ends
take     : (List<T>, Num) -> List<T>
drop     : (List<T>, Num) -> List<T>
count    : (List<T>, (T) -> Flag) -> Num
find     : (List<T>, (T) -> Flag) -> Result<T, Text>
concat   : (List<List<T>>) -> List<T>
reverse  : (List<T>) -> List<T>
repeat   : (Num, T) -> List<T>                -- n copies of item
zip      : (List<T>, List<U>) -> List<(T, U)> -- stops at shorter
sum      : (List<Num>) -> Num
is_empty : (List<T>) -> Flag
head     : (List<T>) -> Result<T, Text>
tail     : (List<T>) -> Result<List<T>, Text>
```

`range(2, 14)` yields `[2, 3, …, 14]`. `lo > hi` returns the empty list.

`take(xs, n)` / `drop(xs, n)` are `split_at`'s two halves in isolation —
pass `n > length(xs)` to take everything / nothing. Negative `n` calls
`fatal`.

`count(xs, f)` = `length(filter(xs, f))`. `find(xs, f)` returns `Ok(x)` for
the first `x` with `f(x) = On`, else `Err("find: no matching element")`.

`concat([[1,2], [3], []]) = [1, 2, 3]`.

`head`/`tail` let you destructure lists without forcing a `match`; useful
when you *know* it's non-empty but don't want a refutable binding.

### 16.2 Numeric predicates and helpers

```
gt, lt, gte, lte, eq_num, ne_num : (Num, Num) -> Flag
between                           : (Num, Num, Num) -> Flag   -- (lo, x, hi), inclusive
min, max                          : (Num, Num) -> Num
is_zero, is_positive, is_negative : (Num) -> Flag
```

These replace `match compare(x, y) { … }` for the common cases. `gt(a, b)`
returns `On` iff `a > b`. `between(lo, x, hi) = On` iff `lo <= x && x <= hi`.

### 16.3 Flag combinators

```
flag_and  : (Flag, Flag) -> Flag
flag_or   : (Flag, Flag) -> Flag
flag_not  : (Flag) -> Flag
when_flag : (Flag, T, T) -> T
```

`when_flag(cond, then_v, else_v)` is a `Flag`-scrutinee branch — equivalent
to `if_eq(cond, On, then_v, else_v)` but reads as what it is. Both
branches evaluate eagerly (same as `if_eq`).

### 16.4 Result helpers

```
require : (Flag, Text) -> Result<Unit, Text>
```

`require(cond, msg) = if flag_is_on(cond) then Ok(Unit) else Err(msg)`.
Designed for validation chains. Most `validate` rules become:

```game
fn validate(view, player, action) =
  let cfg = view_config(view) in
  match cfg { Config { turn, .. } ->
    and_then(require(eq(player, turn), "It is not your turn"), fn (_) ->
      … per-action checks …)
  }
```

### 16.5 Cards

The following builtins assume the common playing-card shape

```
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card { suit: Suit, rank: Num }    -- 2..10, 11=J, 12=Q, 13=K, 14=A
```

Games that declare an exotic `Card` (e.g. a Tarot deck or tile-based game)
simply don't call these. Games that use this shape get them for free:

```
fresh_deck    : () -> List<Card>                         -- the standard 52
card_rank     : (Card) -> Num
card_suit     : (Card) -> Suit
card_has_rank : (Card, Num) -> Flag
card_has_suit : (Card, Suit) -> Flag
cards_of_rank : (List<Card>, Num)  -> List<Card>
cards_of_suit : (List<Card>, Suit) -> List<Card>
```

`fresh_deck()` returns the deck in `Clubs × 2..14, Diamonds × 2..14, …`
order. Call `shuffle_list(rng, fresh_deck())` to get a shuffled deck.

### 16.6 Visibility helper

```
hand_visibility : (PlayerId) -> ((State, PlayerId) -> Visibility)
```

The "player's own hand" pattern. `hand_visibility(p)` returns a visibility
function that yields `SeeAll` when the viewer equals `p`, `SeeSize`
otherwise. Replaces the lambda

```game
fn (state, viewer) -> if_eq(owner, viewer, SeeAll, SeeSize)
```

in a pile declaration:

```game
pile Hand(owner: PlayerId) of Card visibility = hand_visibility(owner)
```

### 16.7 Dealing helpers

```
deal_evenly : (State, List<PlayerId>, List<C>, per: Num,
               pile_of: (PlayerId) -> PileRef<C>)
            -> State

deal_cycle  : (State, List<PlayerId>, List<C>,
               pile_of: (PlayerId) -> PileRef<C>)
            -> State
```

Both take a parameterized pile constructor and a card list:

- `deal_evenly(s, players, cards, 7, Hand)` deals chunks of 7 to each
  player in seat order. If `cards` runs out, the remaining players get
  empty hands.
- `deal_cycle(s, players, cards, Hand)` deals one card at a time cycling
  through `players`, producing round-robin uneven hands when
  `length(cards)` is not a multiple of `length(players)`.

Both are `setup`- and `apply`-callable. Stacking onto a pile that already
has cards appends (per `init_pile`'s semantics).

### 16.8 Deck refill

```
refill : (State, RNG, deck: PileRef<C>, source: PileRef<C>, keep_top: Flag)
       -> State
```

Shuffles `source` into `deck`. If `keep_top = On`, the topmost card of
`source` stays in place (the Crazy Eights pattern where the top of the
discard remains the play reference after refill). No-op when `source`
is empty (or has only the preserved top).

## 17. Availability summary

The matrix below covers only gameplay functions. The text I/O functions (`action_to_text`, `text_to_action`, `view_to_text`, `outcome_to_text`) are each their own callsite with access only to the stdlib functions that match their inputs:

- `action_to_text`, `outcome_to_text`: no state or view access. List ops, comparison, `fatal`, `player_id_to_text`.
- `text_to_action`: view access only (`view_of`, `visible_*`, `view_config`, `players_of_view`), plus generic ops.
- `view_to_text`: same as `text_to_action` — view access only.

| Function | `setup` | `apply` | `validate` | `terminal` | `visibility` | top-level |
|----------|---------|---------|------------|------------|--------------|-----------|
| `cards_in`, `size_of`, `top_of` | ✓ | ✓ | ✗ (no `State`) | ✓ | ✓ | via helpers |
| `view_of`, `visible_*` | ✗ | ✗ | ✓ | ✗ | ✗ | — |
| `move_*`, `shuffle` | ✓ | ✓ | ✗ | ✗ | ✗ | — |
| `new_state`, `init_pile` | ✓ | ✗ | ✗ | ✗ | ✗ | — |
| `temp_pile` | ✗ | ✓ | ✗ | ✗ | ✗ | — |
| `random_int`, `shuffle_list` | ✓ | ✓ | ✗ | ✗ | ✗ | — |
| `players_of` | ✓ | ✓ | ✗ | ✓ | ✓ | — |
| `players_of_view` | ✗ | ✗ | ✓ | ✗ | ✗ | — |
| Config / dict readers | ✓ | ✓ | — (use `view_config`) | ✓ | ✓ | — |
| List ops | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `compare`, `eq`, `if_eq` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `player_id_to_text`, `text_to_player_id` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| `fatal` | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ (but dubious) |

Calling a function outside its availability column (e.g. `temp_pile` from `validate`) is a type error detected at compile time: the required inputs (`State`, `RNG`, etc.) are not in scope.
