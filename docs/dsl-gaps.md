# `.game` DSL — observed gaps

Notes from writing `higher_or_lower`, `old_maid`, `pig`, `blackjack`, and `cheat`
on top of the existing `war` / `go_fish` / `crazy_eights`. These are the rough
edges where every game pays the same boilerplate tax, ordered by how much code
fixing them would actually save.

The goal of the language is "an unfamiliar card game in a couple dozen lines."
Today, the *interesting* part of each ruleset is buried under ~60 lines of
fixed scaffolding.

## 1. Boilerplate every game writes

### 1.1 The standard 52-card deck (≈ 7 lines × every game)

```game
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card { suit: Suit, rank: Num }
let suits = [Clubs, Diamonds, Hearts, Spades]
let ranks = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
fn standard_deck() -> List<Card> =
  flatmap(suits, fn (s) -> map(ranks, fn (r) -> Card { suit: s, rank: r }))
```

8 of 8 example games duplicate this verbatim. The DSL knows nothing about
"playing cards", but we could ship `Suit`, `Card`, `standard_deck`,
`rank_of`, `suit_of`, `has_rank`, `has_suit`, `count_rank` as stdlib so a
`pile Deck of Card visibility = public_size` works without any preamble.
Games that want non-standard decks (Tarot, custom pieces) can still
declare their own `Card` type — the stdlib `Card` would just be the
default everyone reaches for.

### 1.2 The owner-visible hand pattern (every game with hands)

```game
pile Hand(owner: PlayerId) of Card
  visibility = fn (state, viewer) -> if_eq(owner, viewer, SeeAll, SeeSize)
```

Every game with a "hand" pile writes the same lambda. A stdlib helper
`owner_sees_contents_else_size(owner)` (or simply: a `hand_visibility`
constructor) cuts this to one line.

### 1.3 Text-I/O delegation (10 lines × every game)

The four required text functions are *always*:

```game
fn action_to_text(a, p)        = builtin_action_to_text(a, p)
fn text_to_action(t, v, p)     = builtin_text_to_action(t, v, p)
fn view_to_text(v, p)          = builtin_view_to_text(v, p)
fn outcome_to_text(o, p)       = builtin_outcome_to_text(o, p)
```

Make them optional. Default to `builtin_*`. Fall back to a custom
implementation only if declared.

### 1.4 Trivial helpers everyone re-derives

```game
fn rank_of(c)        = match c { Card { rank, .. } -> rank }
fn has_rank(c, r)    = match compare(rank_of(c), r) { EQ -> On; _ -> Off }
fn count_rank(cs, r) = length(filter(cs, fn (c) -> has_rank(c, r)))
```

If the deck is in stdlib (1.1), so are these.

## 2. Missing list / numeric primitives

Every game ends up reinventing one or more of:

| Wanted                       | Workaround used today                              |
| ---------------------------- | -------------------------------------------------- |
| `take(xs, n)`                | `let (taken, _) = split_at(xs, n) in taken`         |
| `drop(xs, n)`                | `let (_, rest) = split_at(xs, n) in rest`           |
| `range(lo, hi)`              | hand-write `[2, 3, …, 14]`                         |
| `count(xs, pred)`            | `length(filter(xs, pred))`                         |
| `find(xs, pred)`             | `filter` then `nth(0)`                             |
| `concat(lol)`                | nested `fold` + `append`                           |
| `reverse(xs)`                | manual recursion                                   |
| `repeat(n, x)`               | manual recursion                                   |
| `n > 0`, `n == 0`            | `match compare(n, 0) { GT -> …; _ -> … }`           |
| `is_empty(list)`             | `match xs { [] -> …; _ -> … }`                     |

Range and the comparison sugar are the two biggest ergonomic wins. The
typechecker already knows `compare : (Num, Num) -> Ordering`; surfacing
`<`, `<=`, `==`, `>=`, `>` operators that desugar to `compare` + a `match`
would cut nested-match counts roughly in half.

## 3. Repeated mechanical patterns

### 3.1 Round-robin / chunked dealing

`old_maid`, `cheat`, `crazy_eights`, `go_fish`, `blackjack` all hand-roll
this. A pair of stdlib calls covers the cases:

```
deal_round_robin : (State, List<PlayerId>, List<C>) -> State
deal_chunks      : (State, List<PlayerId>, List<C>, per: Num) -> State
```

(The complication that **`temp_pile` is apply-only** means setup-time
dealing leans on `init_pile`'s "append to top" semantics, which is
non-obvious. Either lift the apply-only restriction on `temp_pile` or
make stdlib dealing primitives the supported path.)

### 3.2 Deck refill from discard

```game
fn refill_deck(state, rng) =
  let temp = temp_pile() in
  let s1 = move_all(state, from=Used, to=temp) in
  let s2 = shuffle(s1, rng, temp) in
  move_all(s2, from=temp, to=Deck)
```

`crazy_eights` has the more complex variant (keep the top of discard).
Stdlib could ship `refill(state, rng, deck=Deck, source=Used,
keep_top=Off)`.

### 3.3 "Skip done/finished/empty players in the turn cycle"

`crazy_eights` (`next_active`), `old_maid` (`next_active`), `blackjack`
(`next_undone`), `cheat` (similar) all reimplement this. A stdlib
`next_in_cycle_where(players, current, predicate)` retires the
duplication.

### 3.4 "It is not your turn" gate

Every multi-player ruleset opens `validate` with the same shape:

```game
match if_eq(player, turn, On, Off) {
  Off -> Err("It is not your turn");
  On  -> …
}
```

A `require_turn(view, player) -> Result<Unit, Text>` (or syntactic sugar
like a `guard turn == player else "..."` block) would tighten this.

### 3.5 "I hold this card" hand check

```game
match view_of(view, Hand(player)) {
  Contents(cards) ->
    match member(cards, card) {
      On  -> Ok(Unit); Off -> Err("You do not hold that card")
    };
  _ -> fatal("own hand should be visible to owner")
}
```

Every game with a `Play { card }`-shaped action does this. `hand_holds(view,
player, card) -> Result<Unit, Text>` is a clean stdlib function.

## 4. Runtime gaps

### 4.1 `validate` / `apply` / `terminal` can't read `Options`

Today the signatures are:

```
validate(view, player, action) -> ...
apply(state, rng, action, player) -> ...
terminal(state) -> ...
```

If any of those need an option value (a `target_score`, a `dealer_stays_on`,
a `target_streak`, a `max_play`, a `claim_rank` cycle target), it has to be
copied into `Config` at `setup` time and threaded forever after. Every one
of the five new games does this. Either:

- pass `Options` to `validate` / `apply` / `terminal`; or
- expose `options_of(state)` and `view_options(view)` stdlib helpers.

### 4.2 No `terminal` check after `setup`

The engine only polls `terminal` after each `apply`. A game that's already
"settled" at setup (Pig dealt with 4-of-a-kind, Blackjack's pre-deal
naturals) needs at least one move to register the win. Calling `terminal`
once after `setup` would be a one-line engine fix.

### 4.3 `temp_pile` is apply-only

Setup is exactly when you want a scratch pile for shuffling, distributing,
or re-arranging dealt cards. The current workaround leans on `init_pile`'s
"append to top of existing pile" semantics, which works but is subtle.

### 4.4 `outcome_to_text` has no state/view access

Blackjack wants per-player rendering ("you won 21 vs. dealer 19" /
"dealer 19 vs. you 18 — bust"). The signature is `(Outcome, PlayerId) ->
Text` with no view. Workaround: bake all per-player data into the
`Outcome` value (Blackjack does this with
`results: List<(PlayerId, PResult)>`). Acceptable but couples the outcome
type to the renderer's needs.

### 4.5 `PlayerDict` with non-defaultable fields breaks `new_state`

If you want a `PlayerDict { state: PlayerState }` where `PlayerState` is a
sum type, `new_state` fatals (no default constructor). Workaround: keep
`PlayerDict` defaultable and store the state elsewhere, or initialize
every player's dict explicitly via `update_player_dict` after
`new_state`. Documented in stdlib §13 but easy to trip over.

## 5. Surface-syntax wishes

- **Range literal**: `[2..14]` instead of writing all 13 numbers.
- **`if cond then a else b`** for `Flag` scrutinees — `match` is overkill
  for a binary choice.
- **Comparison operators** (`<`, `<=`, `==`, `>=`, `>`) for `Num`, desugared
  to `compare`.
- **`?` / `let?` for `Result`** — chained validation today is a tower of
  nested `match` arms or `and_then` callbacks.
- **Optional declarations** — if a ruleset doesn't override
  `action_to_text` / `text_to_action` / `view_to_text` / `outcome_to_text`,
  pick up the `builtin_*` automatically.
- **Implicit `PlayerDict = PlayerDict {}`** when no per-player state is
  needed; today every empty game declares it explicitly.

## 6. Rough size budget for the "ideal" version

A game with all of the above would have a size budget like:

```
old_maid  →  ~60 lines (vs. 235 today)
pig       →  ~35 lines (vs. 200 today)
cheat     →  ~80 lines (vs. 280 today)
higher_or_lower → ~25 lines (vs. 170 today)
```

The savings come almost entirely from (1) deck-in-stdlib, (2) range
literal, (3) comparison sugar, (4) optional text I/O, and (5) `take`,
`drop`, `count`, `find`, `range`, `is_empty` in the list stdlib. None of
these change the type system or the runtime contract; they're all sugar
and stdlib growth.
