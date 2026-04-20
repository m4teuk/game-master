# `.game` Grammar and Lexical Specification

This document defines the concrete syntax of `.game` files: the tokens a lexer produces and the grammar a parser accepts. It is exhaustive — anything not described here is not part of the language.

Companion docs: [type-system.md](./type-system.md), [runtime.md](./runtime.md), [stdlib.md](./stdlib.md).

---

## 1. Lexical structure

### 1.1 Encoding

Source files are UTF-8. Non-ASCII characters are permitted only inside text literals and comments.

### 1.2 Whitespace

Whitespace characters (`U+0020` space, `U+0009` tab, `U+000A` LF, `U+000D` CR) act as token separators. They are not syntactically significant beyond separation — the language is **not** layout-sensitive.

### 1.3 Comments

A comment begins with `--` and extends to the end of the line.

```
-- this is a comment
let x = 1    -- so is this
```

There is no block-comment syntax in v0.

### 1.4 Keywords

The following identifiers are reserved and cannot be used as user-defined names:

```
fn  let  in  match  type  pile  of  visibility  mod
```

**Soft keywords.** The word `options` introduces an `options_decl` (§2.4) but is not reserved. It is lexed as a `VALUE_IDENT` and recognized by the parser only at top level when immediately followed by `{`. Elsewhere — including parameter names, like `fn setup(..., options: Options, ...)` — it behaves like any other identifier.

### 1.5 Identifiers

Two distinct identifier classes, syntactically enforced by the first character:

- **`TYPE_IDENT`**: starts with an uppercase ASCII letter `[A-Z]`, followed by zero or more characters from `[A-Za-z0-9_]`. Used for type names, constructor names, and pile names.
- **`VALUE_IDENT`**: starts with a lowercase ASCII letter or underscore `[a-z_]`, followed by zero or more characters from `[A-Za-z0-9_]`. Used for function names, parameter names, field names, let-bound names, and type variables.

A single underscore `_` is the **wildcard**; it is not a binding identifier. The wildcard is valid only in pattern positions (`pattern`, §2.8) — using it in an expression position (e.g. `let x = _`, `f(_)`) is a parse error. A `VALUE_IDENT` that begins with `_` followed by more characters (e.g. `_unused`) is a normal binding and is syntactically distinguishable from the wildcard only by length.

### 1.6 Literals

**Numeric literals (`NUM_LIT`):** one or more decimal digits `[0-9]+`. No negative literal form — negation is the unary operator `-` applied to a literal. No floating-point, hex, binary, or octal forms in v0.

**Text literals (`TEXT_LIT`):** delimited by double quotes `"..."`. Supported escape sequences inside:

| Escape | Meaning |
|--------|---------|
| `\\` | backslash |
| `\"` | double quote |
| `\n` | newline |
| `\t` | tab |

No other escapes, no interpolation, no multi-line text literals. A literal newline inside a text literal is a lex error.

### 1.7 Punctuation and operators

```
(  )  {  }  [  ]
,  :  ;  =
->  |  ..  <  >
+  -  *  /
```

`mod` is the one word-shaped infix operator (it is a keyword, §1.4).

`..` is a single token; the lexer produces it as one unit (not two dots).

`->` is a single token.

### 1.8 Token stream

The lexer produces a stream of:

- keywords (§1.4)
- `TYPE_IDENT` and `VALUE_IDENT` (§1.5)
- `NUM_LIT`, `TEXT_LIT` (§1.6)
- punctuation and operators (§1.7)
- end-of-input

No token carries trailing whitespace or comments. Any ambiguity (e.g. `let` as a keyword versus a `VALUE_IDENT`) resolves in favor of the keyword.

---

## 2. Grammar

Metasyntax: `*` is zero-or-more, `+` is one-or-more, `?` is optional, `|` separates alternatives, parentheses group, uppercase names are tokens, lowercase-with-underscore names are nonterminals. Square brackets `[ ]` and curly braces `{ }` in the grammar are **literal** tokens (they appear in the source), not metasyntax.

### 2.1 File

```
file = top_decl*
top_decl = type_decl | pile_decl | options_decl | fn_decl | let_decl
```

Top-level declarations are order-independent. Forward references among them are legal — the engine resolves all names after parsing.

Exactly one `options_decl` is allowed, and it is optional. The required types and functions (listed in [type-system.md §8](./type-system.md)) must each be declared exactly once.

### 2.2 Type declarations

```
type_decl      = "type" TYPE_IDENT "=" "|"? constructor ("|" constructor)*
constructor    = TYPE_IDENT constructor_body?
constructor_body = "{" (field_decl ("," field_decl)* ","?)? "}"
field_decl     = VALUE_IDENT ":" type_expr
```

A constructor without a body is nullary. A constructor with an empty body (`Foo {}`) is also nullary in shape but is a **record-style** declaration, useful when a single-constructor type needs to participate in record construction or update syntax even though it currently has no fields (e.g. `type PlayerDict = PD {}`). A constructor with a non-empty body has one or more named fields. Positional (anonymous) fields are not supported.

The leading `|` before the first constructor is optional and exists purely as a layout convenience for multi-constructor sums:

```
type Phase =
  | ShouldAsk
  | Asked     { target: PlayerId, rank: Num }
  | ShouldDraw { last_rank: Num }
```

A single-constructor type whose constructor shares its name and has fields acts as a record (e.g. `type Config = Config { turn: PlayerId, ... }`).

Trailing comma in the field list is allowed.

### 2.3 Pile declarations

```
pile_decl = "pile" TYPE_IDENT pile_params? "of" type_expr "visibility" "=" expr
pile_params = "(" field_decl ("," field_decl)* ")"
```

The `TYPE_IDENT` becomes both the pile name and a constructor for `PileRef<C>` (where `C` is the card type declared by `of`). See [type-system.md §7](./type-system.md) and [runtime.md §5](./runtime.md) for semantics.

### 2.4 Options declaration

```
options_decl  = "options" "{" (option_field ("," option_field)* ","?)? "}"
option_field  = VALUE_IDENT ":" type_expr "=" expr
```

Each field has a name, a type, and a **required** default value. Field types are restricted to `Num`, `Text`, and user-defined types with all-nullary constructors ([type-system.md §9](./type-system.md)).

### 2.5 Function declarations

```
fn_decl   = "fn" VALUE_IDENT "(" params? ")" "->" type_expr "=" expr
params    = param ("," param)*
param     = VALUE_IDENT (":" type_expr)?
```

Parameter type annotations are optional on individual parameters but must be present on all parameters of any function whose return type involves opaque types (`State`, `View`, `RNG`) or whose parameters interact with them. In practice: the required functions (`setup`, `validate`, `apply`, `terminal`) and their helpers are fully annotated; small helper lambdas can omit annotations. See [type-system.md §5](./type-system.md) for the inference rules.

Function bodies are a single expression (`expr`). Functions are implicitly recursive; mutual recursion across top-level functions is supported.

### 2.6 Let declarations

```
let_decl = "let" pattern "=" expr
```

A top-level `let` declares a constant. It has no `in` clause (distinguishing it from the `let`-expression form §2.9). Top-level lets may reference each other and any function; cycles are detected by the engine and reported as errors.

The LHS is a full [pattern](#2.8-patterns), which lets a single declaration destructure a tuple or single-constructor record: `let (h1, h2) = split_at(xs, 26)`. The pattern must be **irrefutable** (a pattern that always matches its scrutinee's type) — refutable patterns (numeric literals, constructors of sum types, list shapes) are rejected by the type checker ([type-system.md §6.5](./type-system.md)). The common case `let name = expr` is the single-identifier pattern and works as before.

### 2.7 Type expressions

```
type_expr = type_app
          | type_tuple
          | type_fn
          | VALUE_IDENT             -- type variable (stdlib-internal; see §type-system)
type_app   = TYPE_IDENT type_args?
type_args  = "<" type_expr ("," type_expr)* ">"
type_tuple = "(" type_expr ("," type_expr)+ ")"
type_fn    = "(" type_params? ")" "->" type_expr
type_params = type_expr ("," type_expr)*
```

A bare `VALUE_IDENT` in type position is a type variable. Users **cannot introduce** type variables in their own declarations — they exist only in built-in stdlib signatures. The grammar admits them so stdlib signatures can be expressed; user programs that write a `VALUE_IDENT` where a type is expected produce a type error.

### 2.8 Patterns

```
pattern = "_"
        | VALUE_IDENT
        | NUM_LIT
        | TYPE_IDENT pattern_body?
        | TYPE_IDENT "(" pattern ("," pattern)* ","? ")"   -- positional ctor
        | "(" pattern ("," pattern)+ ","? ")"           -- tuple
        | "[" "]"                                       -- empty list
        | "[" pattern ("," pattern)* ","? "]"           -- fixed-length list
        | "[" pattern ("," pattern)* "," ".." VALUE_IDENT? "]"   -- head/rest

pattern_body = "{" field_pat ("," field_pat)* ("," "..")? ","? "}"
field_pat    = VALUE_IDENT (":" pattern)?    -- the short form is field-punning
```

Field-punning: `Card { rank, .. }` is sugar for `Card { rank: rank, .. }` — it matches the `rank` field and binds it to a new variable of the same name.

**Positional constructor patterns** like `Ok(c)`, `Err(_)`, `Contents([t, ..])` provide a shorthand for matching constructors by giving sub-patterns in field-declaration order. The argument list must be non-empty (use the bare `Ctor` form for nullary constructors); arity must match the constructor's declared field count exactly. Most commonly used with the built-in single-field constructors of `Result<T, E>`, `PileView<C>`, and `GameStatus<R>`. Type-system rules in [type-system.md §6.1](./type-system.md).

The `..` after field-pats in a `TYPE_IDENT` pattern means "ignore remaining fields." Without `..`, all fields must be mentioned.

Literal patterns: `NUM_LIT` matches equal `Num`s. A nullary `TYPE_IDENT` pattern matches exactly that constructor (e.g. `Spades` in a `Suit`-typed match).

Patterns may nest to any depth. Exhaustiveness is checked by the type system (see [type-system.md §6](./type-system.md)).

### 2.9 Expressions

Precedence levels, from loosest to tightest:

1. **Spine forms** (`let`, `match`, `fn`): extend rightward as far as possible.
2. **Additive binary**: `+`, `-`
3. **Multiplicative binary**: `*`, `/`, `mod`
4. **Unary**: `-`
5. **Application**: `f(…)` — left-associative
6. **Atomic**: literals, identifiers, parenthesized, collection/record literals

All binary operators are **left-associative**.

```
expr = expr_spine

expr_spine = expr_let
           | expr_match
           | expr_lambda
           | expr_add

expr_let    = "let" pattern "=" expr_spine "in" expr_spine
expr_match  = "match" expr "{" match_arm (";" match_arm)* ";"? "}"
match_arm   = pattern "->" expr_spine
expr_lambda = "fn" "(" params? ")" "->" expr_spine

expr_add = expr_add ("+" | "-") expr_mul | expr_mul
expr_mul = expr_mul ("*" | "/" | "mod") expr_unary | expr_unary
expr_unary = "-" expr_unary | expr_app

expr_app = expr_app "(" (arg ("," arg)* ","?)? ")"
         | expr_atom
arg      = expr | VALUE_IDENT "=" expr   -- positional or keyword

expr_atom = NUM_LIT | TEXT_LIT
          | VALUE_IDENT
          | TYPE_IDENT                                  -- nullary constructor
          | TYPE_IDENT "{" record_body "}"              -- tagged record (construction)
          | "(" expr ")"                                -- parenthesization
          | "(" expr ("," expr)+ ","? ")"               -- tuple literal
          | "[" "]"                                     -- empty list
          | "[" expr ("," expr)* ","? "]"               -- list literal

record_body = (".." expr ",")? field_init ("," field_init)* ","?
field_init  = VALUE_IDENT ":" expr
            | VALUE_IDENT                               -- field-punning (short for name: name)
```

Notes:

- **Record construction vs. record update.** Construction: `Config { turn: p, phase: ShouldAsk, … }` — all fields required. Update: `Config { ..cfg, phase: ShouldAsk }` — the spread is optional but when present must appear first. Update fields override the spread; omitted fields inherit from the spread.
- **Function calls accept keyword arguments.** `move_card(s, from=Deck, to=Hand(p), card=c)`. Keyword args may be interleaved with positional args; the engine reorders by name. Each parameter may be passed at most once.
- **Match arms are separated by `;`**, with an optional trailing `;`. There is no layout rule.
- **Lambda expressions** extend rightward as far as possible: `fn (x) -> x + 1` parses `x + 1` as the whole body.
- **Let expression** (§2.9, as opposed to §2.6 top-level let) uses `in`: `let pat = expr1 in expr2`. The LHS is a pattern (same form as §2.8) and must be irrefutable — see [type-system.md §6.5](./type-system.md). In the common single-identifier case, it reads as the familiar `let name = …`.

### 2.10 Disambiguation of `<` and `>`

These characters appear both as type-argument brackets (`List<T>`) and as the future home of comparison operators (not in v0). In v0 they are **only** type-argument brackets and therefore unambiguous — they appear only after a `TYPE_IDENT` in type-expression position.

### 2.11 Reserved but unused

The following tokens/words are reserved and will be rejected by the parser if encountered outside the uses given above:

- `..` (only inside list patterns and record spreads)
- `->` (only in function/lambda/match)
- `|>` (reserved for future pipe; currently unused — using it is a parse error)

---

## 3. Worked example

A minimal parseable file:

```
type Suit    = Clubs | Diamonds | Hearts | Spades
type Card    = Card { suit: Suit, rank: Num }
type Action  = PlayCard { card: Card } | Pass
type Outcome = Winner { player: PlayerId }
type Config  = Config { turn: PlayerId, top: Card }
type PlayerDict = PD {}

pile Deck    of Card visibility = public_size
pile Hand(owner: PlayerId) of Card
  visibility = fn (state, viewer) -> if_eq(owner, viewer, SeeAll, SeeSize)
pile Discard of Card visibility = public

let ranks = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]

fn setup(players: List<PlayerId>, options: Options, rng: RNG)
  -> Result<State, Text> =
  match players {
    [first, ..] ->
      let cfg = Config { turn: first, top: Card { suit: Clubs, rank: 2 } } in
      Ok(new_state(cfg));
    [] -> Err("at least one player required")
  }

fn validate(view: View, player: PlayerId, action: Action)
  -> Result<Unit, Text> = Ok(Unit)

fn apply(state: State, rng: RNG, action: Action, player: PlayerId)
  -> State = state

fn terminal(state: State) -> GameStatus<Outcome> = Ongoing
```

Every form in §2 appears here. A correct lexer + parser produces one `file` AST with six `type_decl`s, three `pile_decl`s, one `let_decl`, and four `fn_decl`s, in any order.
