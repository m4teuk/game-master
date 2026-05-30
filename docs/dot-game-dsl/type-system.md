# `.game` Type System

This document specifies how types are assigned, inferred, and checked. It assumes familiarity with [grammar.md](./grammar.md) for the surface syntax.

Companion docs: [grammar.md](./grammar.md), [runtime.md](./runtime.md), [stdlib.md](./stdlib.md).

---

## 1. Type universe

Every expression has exactly one type. Types fall into four categories:

1. **Built-in types**: `Num`, `Text`, `PlayerId`, `Unit`, `RNG`, `State`, `View`, `Options`. Non-generic.
2. **Built-in generic types**: `List<T>`, `Result<T, E>`, `GameStatus<R>`, `PileView<C>`, `PileRef<C>`. The only generic types that exist.
3. **Built-in algebraic types**: `Visibility`, `Ordering`, `Flag`, and the constructors of the generic built-ins (`Ok`, `Err`, `Ongoing`, `Ended`, `Contents`, `Size`, `Masked`, `LT`, `EQ`, `GT`, `On`, `Off`, `SeeAll`, `SeeSize`, `Hidden`).
4. **Tuple types**: `(A, B)`, `(A, B, C)`, … — structural, any arity ≥ 2.
5. **User-declared algebraic types**: declared by `type` (§3).

Functions have function types, written `(A, B, …) -> R`. Function types are **not** first-class in user code beyond being passed as arguments and returned — users cannot declare a type alias for a function type.

### 1.1 Opaque types

`State`, `View`, `RNG`, and `PileRef<C>` are **opaque**:

- Users cannot pattern-match against them.
- Users cannot construct them directly (no constructor syntax). `State` and `View` come from the engine; `RNG` is passed in; `PileRef<C>` values come from pile-name constructors introduced by `pile` declarations ([runtime.md §5](./runtime.md)).
- Users cannot store them inside user-declared types. A field of type `State` or `View` is a type error.

### 1.2 The `Options` type

`Options` is synthesized by the engine from the `options { … }` block as a single-constructor record:

- If the block is absent, the synthesized type has zero fields — equivalent to `type Options = Options { }`. The sole value is `Options { }`, and pattern destructuring is `match opts { Options { } -> … }` (or `Options { .. }` interchangeably).
- Otherwise a single-constructor algebraic type with one field per option declaration, whose field names and types match the block.

The synthesized name is always `Options` and the constructor is always `Options` (same name). Users destructure it with ordinary pattern matching: `match opts { Options { target_score, .. } -> … }`. In practice, rulesets without an `options` block simply ignore the parameter and never reference the type.

---

## 2. Type variables

A bare `VALUE_IDENT` in a type-expression position is a **type variable**. Type variables are implicitly universally quantified at the signature they appear in.

User-written types **may not contain type variables.** The grammar admits them so that stdlib signatures can be expressed (e.g. `map : (List<T>, (T) -> U) -> List<U>`). A user program that writes a `VALUE_IDENT` in type position is a type error: `users cannot define generic types in v0`.

Stdlib type variables are resolved per-call. At each call site, the type checker infers a concrete type for each variable from the argument types; the return type is then the variable-substituted result type. This is standard Hindley-Milner-style instantiation restricted to predeclared signatures.

Type-variable names, as a lexical convention in stdlib signatures: `T`, `U`, `C`, `E`, `R`, `Acc`, `a`. These are not reserved — they are just `VALUE_IDENT`s that appear in type position.

---

## 3. User-declared types

```
type Suit  = Clubs | Diamonds | Hearts | Spades
type Card  = Card { suit: Suit, rank: Num }
type Phase = ShouldAsk | Asked { target: PlayerId, rank: Num } | ShouldDraw { last_rank: Num }
```

A type declaration introduces one type name and one or more **constructor names**. Constructor names share the `TYPE_IDENT` namespace with type names, but a constructor is not a type — it is a value of its declaring type. Two types may not share a constructor name; two constructor names within the same file must be distinct.

Fields of a constructor are named (§2.2 of grammar). Within one constructor, field names must be distinct. Two constructors in the same type may share a field name only if the field types are identical — but this is convention, not a rule; the type checker treats each constructor's fields independently.

Recursive and mutually recursive type definitions are allowed (e.g. `type Tree = Leaf | Node { left: Tree, right: Tree }`). The type checker resolves names in a single pass after parsing.

### 3.1 Single-constructor record form

When a type has exactly one constructor and the constructor name equals the type name, the declaration defines a **record**. `Config { turn: PlayerId, phase: Phase }` is both the type and the construction form; `match cfg { Config { turn, phase } -> … }` destructures it.

Records are not structurally typed — `Config { turn, phase }` and `Snapshot { turn, phase }` with the same fields are distinct, incompatible types.

### 3.2 Required user types

A valid `.game` file must declare these five:

| Name | Role |
|------|------|
| `Card` | The game's card type. Used as the `C` in `PileRef<C>` throughout. |
| `Action` | Sum of all player moves. |
| `Outcome` | Game result; the `R` in `GameStatus<R>` returned by `terminal`. |
| `Config` | Single-constructor record; the global public state schema. |
| `PlayerDict` | Single-constructor record; per-player public state schema. May have zero fields. |

Declaring these with the wrong shape (e.g. `Config` as a multi-constructor sum) is a type error.

### 3.3 Required functions

A valid `.game` file must declare these functions with exactly these signatures:

```
fn setup         : (List<PlayerId>, Options, RNG) -> Result<State, Text>
fn validate      : (View, PlayerId, Action)       -> Result<Unit, Text>
fn apply         : (State, RNG, Action, PlayerId) -> State
fn terminal      : (State)                        -> GameStatus<Outcome>

fn action_to_text   : (Action, PlayerId)           -> Text
fn text_to_action   : (Text, View, PlayerId)       -> Result<Action, Text>
fn view_to_text     : (View, PlayerId)             -> Text
fn outcome_to_text  : (Outcome, PlayerId)          -> Text
```

Plus per-pile `visibility : (State, PlayerId) -> Visibility`.

Missing any of these, or declaring any with a different signature, is a type error. The engine rejects the ruleset at load time.

---

## 4. Types of expressions

Each expression form has a typing rule. Informally:

- **Literals.** `NUM_LIT : Num`. `TEXT_LIT : Text`.
- **Identifiers.** A `VALUE_IDENT` has the type of its binding (function parameter, `let`, or top-level). A `TYPE_IDENT` has the type/arity of its constructor.
- **Nullary constructor.** `C : T` if `C` is a nullary constructor of type `T`.
- **Constructor with fields.** `C { f1: e1, f2: e2, … } : T` if `C` is a constructor of `T` with exactly fields `f1, f2, …` and `ei : Ti` where `Ti` is the declared type of `fi`.
- **Record update.** `C { ..e0, f1: e1, … } : T` if `e0 : T`, `C` is `T`'s record constructor, and each overriding `ei` matches its field's type. Fields not overridden are inherited from `e0`.
- **Tuple literal.** `(e1, e2, …) : (T1, T2, …)` if each `ei : Ti`.
- **List literal.** `[e1, e2, …] : List<T>` if every `ei : T`. The empty list `[]` has type `List<T>` for some `T` determined by context.
- **Function call.** `f(a1, a2, …)` where `f : (P1, …, Pn) -> R` requires each `ai : Pi` after keyword-argument reordering. The call has type `R`.
- **Lambda.** `fn (x: A, y: B) -> body` has type `(A, B) -> R` where `R` is the type of `body`. Parameter annotations may be omitted when the target type is known from context (see §5).
- **Let.** `let pat = e1 in e2` has the type of `e2`. `pat` is matched against `e1` (whose type determines the types of all variables bound by `pat`), and those bindings are in scope throughout `e2`. The pattern must be **irrefutable** (§6.5); otherwise the type checker rejects the program. The common single-identifier case `let x = e1 in e2` is exactly this rule with `pat = x`.
- **Match.** `match e { p1 -> e1; p2 -> e2; … }` requires each `pi` to match the type of `e` and every `ei` to have the same type `R`; the whole expression has type `R`.
- **Unary `-`.** `-e : Num` if `e : Num`.
- **Binary arithmetic.** `e1 op e2 : Num` if `e1 : Num` and `e2 : Num`, for `op ∈ {+, -, *, /, mod}`.

---

## 5. Type inference

The language is **locally inferred, not globally Hindley-Milner**. Rules:

1. Every top-level function declaration must annotate its return type and all parameter types. Exception: parameter annotations may be omitted if the function is the inline visibility function of a pile, in which case the expected type `(State, PlayerId) -> Visibility` provides the annotations.
2. Lambda parameters may be annotated or inferred. A lambda appearing in an argument position to a function with a known expected type (e.g. the callback to `map`, `filter`, `fold`) infers its parameter types from the callee's signature. Otherwise parameters must be annotated.
3. `let pat = e in body` (and top-level `let pat = e`) infers the types of all variables bound by `pat` from `e`'s type, by matching the pattern shape against the type. No annotation syntax on lets in v0. The pattern must be irrefutable (§6.5).
4. Empty list `[]` and empty collections infer their element type from context. If unconstrained, it is a type error.

In practice: fully annotate `fn` declarations; let the inference handle lambdas passed to higher-order functions.

> **v0 limitation.** Type variables that flow out of an unconstrained
> generic call (e.g. `let p = temp_pile() in …`) are not unified
> across uses — each subsequent use solves them locally and the
> per-use solutions are independent. The same `p` could therefore
> typecheck against two incompatible card types. Don't bind generic
> stdlib calls in a `let` and reuse the binding heterogeneously;
> apply them inline at each call site instead. A future version
> will allocate fresh metavariables and solve them globally.

---

## 6. Pattern matching

### 6.1 Pattern typing

Every pattern has a type, determined by its shape:

- `_` matches any type.
- A `VALUE_IDENT` matches any type and binds it.
- A `NUM_LIT` requires the scrutinee to have type `Num`.
- A nullary `TYPE_IDENT` requires the scrutinee to have that constructor's declaring type.
- `TYPE_IDENT { field: subpat, … }` requires the scrutinee to have that constructor's declaring type; each `field` must be one of the constructor's fields; `subpat` must match the field's type. Unmentioned fields must be covered by `..` or explicitly listed.
- `TYPE_IDENT(p1, …, pn)` (positional form) is equivalent to the named-field form `TYPE_IDENT { f1: p1, …, fn: pn }` where `f1, …, fn` are the constructor's fields in declaration order. Arity must match exactly; `..` is not available in this form. Most commonly used with built-in single-field constructors (`Ok(c)`, `Err(e)`, `Contents(cs)`, `Size(n)`, `Ended(r)`).
- A tuple pattern `(p1, p2, …)` requires a tuple type `(T1, T2, …)` of the same arity.
- List patterns `[]`, `[p1, …, pn]`, `[p1, …, pn, ..rest]` require a `List<T>`; subpatterns match `T`; `rest` (if named) binds `List<T>`.

### 6.2 Exhaustiveness

Every `match` expression must be **exhaustive**. Non-exhaustive matches are a type error detected at compile time.

Exhaustiveness is checked per scrutinee type:

- Scrutinees of algebraic types must cover every constructor.
- Scrutinees of `Num` or `Text` must end in a wildcard `_` or value-binding pattern.
- Scrutinees of `List<T>` must cover `[]` (or a pattern that matches it) and all non-empty cases; using a head/rest pattern `[x, ..xs]` covers all non-empty lists.
- Scrutinees of tuples must cover the tuple shape exhaustively — typically one catch-all arm suffices.
- Scrutinees of opaque types cannot be matched at all (no patterns are valid); the type checker rejects such matches.

### 6.3 Reachability and overlap

Patterns are tried top-to-bottom. A pattern that can never match because an earlier pattern subsumes it is a **warning** in a future version (not an error). v0 does not yet emit these warnings — the [Tc_errors] module currently has only an error severity. Adding a warning surface and the Maranget-style matrix algorithm needed to detect non-trivial overlaps is deferred.

### 6.4 Runtime behavior

If exhaustiveness is enforced statically, no runtime "no match" case exists. The engine implements match as a direct jump to the first matching arm; if (due to an engine bug) no arm matches, it calls `fatal`.

### 6.5 Irrefutable patterns

A pattern is **irrefutable** at a given scrutinee type `T` if it is guaranteed to match every value of `T`. Irrefutable patterns appear on the LHS of `let` bindings — both the expression form (`let pat = e1 in e2`, [grammar.md §2.9](./grammar.md)) and the top-level form (`let pat = e`, [grammar.md §2.6](./grammar.md)). Using a refutable pattern in those positions is a compile-time error.

At scrutinee type `T`, a pattern is irrefutable iff it is one of:

- `_` (the wildcard).
- A `VALUE_IDENT` (binding pattern).
- A tuple pattern `(p1, …, pn)` where every `pi` is irrefutable at the corresponding component type. (Tuple types always have a single shape, so the shell always matches.)
- A constructor pattern `C { f1: p1, …, fn: pn }` (optionally with `..`) where `T` is a **single-constructor** algebraic type (i.e. a record) with constructor `C`, and every `pi` is irrefutable at its field type. The positional form `C(p1, …, pn)` is treated the same way (it is just the named form with implicit field ordering).

Everything else is refutable:

- `NUM_LIT` patterns (a specific number may not match).
- Nullary constructor patterns of sum types with more than one constructor.
- Constructor patterns on sum types with more than one constructor.
- All list patterns (`[]`, `[p1, …, pn]`, `[p1, …, pn, ..rest]`) — the empty vs. non-empty distinction is always a branch.

Match expressions (§6.1–§6.4) accept refutable patterns freely; irrefutability is a constraint on `let` bindings only. Most practical uses are a single identifier (`let count = …`), a tuple destructure (`let (h1, h2) = split_at(xs, 26)`), or a record destructure (`let Config { turn, .. } = cfg`).

---

## 7. Equality and ordering

### 7.1 Equality

The built-in `eq(a, b) -> Flag` and `if_eq(a, b, then, else)` work for values of the same type if that type is **equality-admissible**. A type is equality-admissible iff:

- It is `Num`, `Text`, `PlayerId`, or `Unit`.
- It is a built-in algebraic type (`Flag`, `Ordering`, `Visibility`, `Result<T, E>` if `T` and `E` are, `GameStatus<R>` if `R` is, `PileView<C>` if `C` is).
- It is a tuple type whose components all are.
- It is `List<T>` if `T` is.
- It is a user-declared algebraic type all of whose constructors' fields are equality-admissible.

A type is **not** equality-admissible if it contains `State`, `View`, `RNG`, `PileRef<C>`, a function type, or (transitively) anything not on the list above.

Equality of algebraic values is structural: same constructor AND all fields pairwise equal. `Num` uses numeric equality, `Text` uses code-point equality, `PlayerId` uses engine-opaque equality (two `PlayerId`s are equal iff they represent the same player).

Attempting `eq` on a non-admissible type is a type error.

### 7.2 Ordering

`compare(a: Num, b: Num) -> Ordering` is defined only for `Num`. No other ordering is provided. Users who need ordering over other types implement it themselves.

---

## 8. Scoping and name resolution

### 8.1 Namespaces

Three namespaces, resolved separately:

1. **Type-level**: type names. Populated by built-ins and `type` declarations.
2. **Constructor-level**: constructor names. Populated by built-ins and all constructors of all `type` declarations.
3. **Value-level**: functions, top-level lets, function parameters, let-bound variables, pattern-bound variables, and **pile names**. A pile declaration `pile Hand(owner: PlayerId) of Card` introduces `Hand` as a value of type `(PlayerId) -> PileRef<Card>` (or `PileRef<C>` when the pile has no parameters). Although pile names are `TYPE_IDENT`-shaped at the syntax level, semantically they construct opaque `PileRef<C>` handles via ordinary application — they are never matched in patterns (per §1.1) and so live in the value namespace, not the constructor namespace.

A name may appear in more than one namespace (e.g. `Config` is both a type and a constructor). The parser disambiguates from context: types appear in type-expression positions, constructors appear in patterns and expressions.

### 8.2 Shadowing

- Pattern-bound and let-bound variables may shadow outer value-level bindings of the same name.
- Function parameters shadow outer value-level bindings.
- Type and constructor names may **not** be shadowed — a user `type` declaration that reuses a built-in name is an error.
- Top-level names (functions, top-level lets) share one namespace; duplicates are an error.

### 8.3 Forward references

All top-level declarations are visible to all others. Functions may call functions declared later in the file. Top-level lets may reference functions and other lets in any order; the engine detects value-level cycles (a let that directly or transitively depends on itself) as an error.

---

## 9. Options type restrictions

Fields in the `options { … }` block are restricted:

- Field types allowed: `Num`, `Text`, and user-declared types whose constructors are **all nullary** (enum-like).
- Nested records are not allowed. Lists and tuples are not allowed. Built-in algebraic types like `Flag` are allowed (all-nullary). `Result<T, E>`, `GameStatus<R>`, etc. are not.
- Every field must have a default value expression of the correct type. The default is evaluated once at engine-form-generation time.

These restrictions exist so the engine can render options as a CLI prompt, TUI form, or JSON object without having to handle arbitrary user types.

---

## 10. Error model

The type checker reports:

- **Errors**: declarations are rejected. Compilation fails.
- **Warnings**: emitted, compilation proceeds.

Error categories:

- Name resolution (unknown type, unknown constructor, unknown value, duplicate).
- Arity mismatch (wrong number of arguments, fields, tuple components).
- Type mismatch (expression type ≠ expected type).
- Non-admissible equality/comparison.
- Non-exhaustive match.
- Refutable pattern in a `let` binding (§6.5).
- Type variable appearing in user declaration.
- Opaque type used in disallowed position (field of user type, pattern scrutinee).
- Missing required declaration (e.g. no `setup` function).
- Signature mismatch on required functions (`setup`, `validate`, `apply`, `terminal`).
- Multiple `options` declarations.
- Top-level value cycle.

Warning categories:

- Redundant / unreachable pattern.
- Unused binding.

All error messages include source location (line, column) from the token stream.
