open Core

let nullary owner name : Types.ctor_info =
  { ctor_name = name; owner_type = owner; fields = []; is_record = false }

let with_fields owner name fields : Types.ctor_info =
  { ctor_name = name; owner_type = owner; fields; is_record = false }

let add_adt env ~name ~ctors =
  let env = Types.add_type env name (Types.TI_adt { ctors; is_record = false }) in
  List.fold ctors ~init:env ~f:(fun env c ->
    Types.add_ctor env c.Types.ctor_name c)

let seed_types env =
  (* Unit as a single-ctor type so users can write [Ok(Unit)] / pattern
     match. T_unit and T_user "Unit" unify in [Types.unify]. *)
  let env = add_adt env ~name:"Unit" ~ctors:[
    nullary "Unit" "Unit";
  ] in
  let env = add_adt env ~name:"Visibility" ~ctors:[
    nullary "Visibility" "SeeAll";
    nullary "Visibility" "SeeSize";
    nullary "Visibility" "Hidden";
  ] in
  let env = add_adt env ~name:"Ordering" ~ctors:[
    nullary "Ordering" "LT";
    nullary "Ordering" "EQ";
    nullary "Ordering" "GT";
  ] in
  let env = add_adt env ~name:"Flag" ~ctors:[
    nullary "Flag" "On";
    nullary "Flag" "Off";
  ] in
  let env = add_adt env ~name:"Result" ~ctors:[
    with_fields "Result" "Ok"  [("value", Types.T_var 0)];
    with_fields "Result" "Err" [("error", Types.T_var 1)];
  ] in
  let env = add_adt env ~name:"GameStatus" ~ctors:[
    nullary "GameStatus" "Ongoing";
    with_fields "GameStatus" "Ended" [("outcome", Types.T_var 0)];
  ] in
  let env = add_adt env ~name:"PileView" ~ctors:[
    with_fields "PileView" "Contents" [("items", Types.T_list (Types.T_var 0))];
    with_fields "PileView" "Size"     [("n", Types.T_num)];
    nullary     "PileView" "Masked";
  ] in
  env

(* ---------------------------------------------------------------- *)
(* stdlib value bindings — see stdlib.md                             *)
(* ---------------------------------------------------------------- *)

(* [add_fn] takes named parameters so keyword-arg call sites can
   resolve names per stdlib spec (e.g. [move_top(s, from=..., to=...)]).
   The names also serve as documentation when reading the seed table. *)
let add_fn name (params : (string * Types.ty) list) ret env =
  let tys = List.map params ~f:snd in
  let names = List.map params ~f:fst in
  let env = Types.add_value env name (Types.T_fn (tys, ret)) in
  Types.add_param_names env name names

(* Convenience aliases *)
let v i = Types.T_var i
let list t = Types.T_list t
let result t e = Types.T_result (t, e)
let pile_view t = Types.T_pile_view t
let pile_ref t = Types.T_pile_ref t
let user n = Types.T_user n
let fn args ret = Types.T_fn (args, ret)
let tuple ts = Types.T_tuple ts

let seed_values env =
  let env =
    (* §1 Pile access (server-side) *)
    env
    |> add_fn "cards_in"  [("state", Types.T_state); ("pile", pile_ref (v 0))] (list (v 0))
    |> add_fn "size_of"   [("state", Types.T_state); ("pile", pile_ref (v 0))] Types.T_num
    |> add_fn "top_of"    [("state", Types.T_state); ("pile", pile_ref (v 0))] (result (v 0) Types.T_text)

    (* §2 Pile access (view-side) *)
    |> add_fn "view_of"          [("view", Types.T_view); ("pile", pile_ref (v 0))] (pile_view (v 0))
    |> add_fn "visible_size_of"  [("view", Types.T_view); ("pile", pile_ref (v 0))] (result Types.T_num Types.T_text)
    |> add_fn "visible_top_of"   [("view", Types.T_view); ("pile", pile_ref (v 0))] (result (v 0) Types.T_text)

    (* §3 Pile mutation — names per stdlib spec ([from], [to], [card]). *)
    |> add_fn "move_top"
         [("state", Types.T_state); ("from", pile_ref (v 0)); ("to", pile_ref (v 0))]
         Types.T_state
    |> add_fn "move_card"
         [("state", Types.T_state); ("from", pile_ref (v 0));
          ("to", pile_ref (v 0)); ("card", v 0)]
         Types.T_state
    |> add_fn "move_to_bottom"
         [("state", Types.T_state); ("from", pile_ref (v 0)); ("to", pile_ref (v 0))]
         Types.T_state
    |> add_fn "move_all"
         [("state", Types.T_state); ("from", pile_ref (v 0)); ("to", pile_ref (v 0))]
         Types.T_state
    |> add_fn "move_all_to_bottom"
         [("state", Types.T_state); ("from", pile_ref (v 0)); ("to", pile_ref (v 0))]
         Types.T_state
    |> add_fn "shuffle"
         [("state", Types.T_state); ("rng", Types.T_rng); ("pile", pile_ref (v 0))]
         Types.T_state

    (* §4 Setup-only *)
    |> add_fn "new_state" [("config", user "Config")] Types.T_state
    |> add_fn "init_pile"
         [("state", Types.T_state); ("pile", pile_ref (v 0)); ("cards", list (v 0))]
         Types.T_state

    (* §5 Apply-only *)
    |> add_fn "temp_pile" [] (pile_ref (v 0))

    (* §6 Config, player dict, roster *)
    |> add_fn "config_of"          [("state", Types.T_state)] (user "Config")
    |> add_fn "view_config"        [("view", Types.T_view)] (user "Config")
    |> add_fn "with_config"        [("state", Types.T_state); ("config", user "Config")] Types.T_state
    |> add_fn "player_dict"        [("state", Types.T_state); ("player", Types.T_player_id)] (user "PlayerDict")
    |> add_fn "update_player_dict"
         [("state", Types.T_state); ("player", Types.T_player_id);
          ("update", fn [user "PlayerDict"] (user "PlayerDict"))]
         Types.T_state
    |> add_fn "players_of"         [("state", Types.T_state)] (list Types.T_player_id)
    |> add_fn "players_of_view"    [("view", Types.T_view)]   (list Types.T_player_id)

    (* §7 RNG *)
    |> add_fn "random_int"   [("rng", Types.T_rng); ("lo", Types.T_num); ("hi", Types.T_num)] Types.T_num
    |> add_fn "shuffle_list" [("rng", Types.T_rng); ("list", list (v 0))] (list (v 0))

    (* §8 Visibility helpers *)
    |> add_fn "public"      [("state", Types.T_state); ("player", Types.T_player_id)] (user "Visibility")
    |> add_fn "public_size" [("state", Types.T_state); ("player", Types.T_player_id)] (user "Visibility")
    |> add_fn "hidden"      [("state", Types.T_state); ("player", Types.T_player_id)] (user "Visibility")
    |> add_fn "owner_only"  [("owner", Types.T_player_id)]
                            (fn [Types.T_state; Types.T_player_id] (user "Visibility"))

    (* §9 Lists *)
    |> add_fn "length"        [("list", list (v 0))] Types.T_num
    |> add_fn "map"           [("list", list (v 0)); ("f", fn [v 0] (v 1))] (list (v 1))
    |> add_fn "filter"        [("list", list (v 0)); ("f", fn [v 0] (user "Flag"))] (list (v 0))
    |> add_fn "fold"          [("list", list (v 0)); ("acc", v 1); ("f", fn [v 1; v 0] (v 1))] (v 1)
    |> add_fn "flatmap"       [("list", list (v 0)); ("f", fn [v 0] (list (v 1)))] (list (v 1))
    |> add_fn "append"        [("xs", list (v 0)); ("ys", list (v 0))] (list (v 0))
    |> add_fn "nth"           [("list", list (v 0)); ("index", Types.T_num)] (result (v 0) Types.T_text)
    |> add_fn "member"        [("list", list (v 0)); ("item", v 0)] (user "Flag")
    |> add_fn "any"           [("list", list (v 0)); ("f", fn [v 0] (user "Flag"))] (user "Flag")
    |> add_fn "all"           [("list", list (v 0)); ("f", fn [v 0] (user "Flag"))] (user "Flag")
    |> add_fn "split_at"      [("list", list (v 0)); ("n", Types.T_num)] (tuple [list (v 0); list (v 0)])
    |> add_fn "next_in_cycle" [("list", list (v 0)); ("item", v 0)] (v 0)

    (* §10 Result *)
    |> add_fn "ok"       [("value", v 0)] (result (v 0) (v 1))
    |> add_fn "err"      [("error", v 1)] (result (v 0) (v 1))
    |> add_fn "and_then" [("result", result (v 0) (v 1)); ("f", fn [v 0] (result (v 2) (v 1)))]
                         (result (v 2) (v 1))

    (* §11 Comparison and branching *)
    |> add_fn "compare" [("a", Types.T_num); ("b", Types.T_num)] (user "Ordering")
    |> add_fn "eq"      [("a", v 0); ("b", v 0)] (user "Flag")
    |> add_fn "if_eq"   [("a", v 0); ("b", v 0); ("then_", v 1); ("else_", v 1)] (v 1)

    (* §12 Fatal — return type is a free variable, unifies with anything *)
    |> add_fn "fatal" [("message", Types.T_text)] (v 0)

    (* §14 Text rendering built-ins *)
    |> add_fn "builtin_action_to_text"  [("action", user "Action"); ("player", Types.T_player_id)] Types.T_text
    |> add_fn "builtin_text_to_action"  [("text", Types.T_text); ("view", Types.T_view); ("player", Types.T_player_id)]
                                        (result (user "Action") Types.T_text)
    |> add_fn "builtin_view_to_text"    [("view", Types.T_view); ("player", Types.T_player_id)] Types.T_text
    |> add_fn "builtin_outcome_to_text" [("outcome", user "Outcome"); ("player", Types.T_player_id)] Types.T_text
    |> add_fn "builtin_action_to_json"  [("action", user "Action"); ("player", Types.T_player_id)] Types.T_text
    |> add_fn "builtin_json_to_action"  [("text", Types.T_text); ("view", Types.T_view); ("player", Types.T_player_id)]
                                        (result (user "Action") Types.T_text)
    |> add_fn "builtin_view_to_json"    [("view", Types.T_view); ("player", Types.T_player_id)] Types.T_text
    |> add_fn "builtin_outcome_to_json" [("outcome", user "Outcome"); ("player", Types.T_player_id)] Types.T_text

    (* §15 PlayerId conversion *)
    |> add_fn "player_id_to_text" [("player", Types.T_player_id)] Types.T_text
    |> add_fn "text_to_player_id" [("text", Types.T_text); ("view", Types.T_view)] (result Types.T_player_id Types.T_text)

    (* ============================================================ *)
    (* §16 Extended stdlib — non-breaking additions (see stdlib.md)  *)
    (* ============================================================ *)

    (* §16.1 List ops *)
    |> add_fn "range"    [("lo", Types.T_num); ("hi", Types.T_num)] (list Types.T_num)
    |> add_fn "take"     [("list", list (v 0)); ("n", Types.T_num)] (list (v 0))
    |> add_fn "drop"     [("list", list (v 0)); ("n", Types.T_num)] (list (v 0))
    |> add_fn "count"    [("list", list (v 0)); ("f", fn [v 0] (user "Flag"))] Types.T_num
    |> add_fn "find"     [("list", list (v 0)); ("f", fn [v 0] (user "Flag"))]
                         (result (v 0) Types.T_text)
    |> add_fn "concat"   [("list", list (list (v 0)))] (list (v 0))
    |> add_fn "reverse"  [("list", list (v 0))] (list (v 0))
    |> add_fn "repeat"   [("n", Types.T_num); ("item", v 0)] (list (v 0))
    |> add_fn "zip"      [("xs", list (v 0)); ("ys", list (v 1))]
                         (list (tuple [v 0; v 1]))
    |> add_fn "sum"      [("list", list Types.T_num)] Types.T_num
    |> add_fn "is_empty" [("list", list (v 0))] (user "Flag")
    |> add_fn "head"     [("list", list (v 0))] (result (v 0) Types.T_text)
    |> add_fn "tail"     [("list", list (v 0))] (result (list (v 0)) Types.T_text)

    (* §16.2 Numeric predicates and helpers *)
    |> add_fn "gt"      [("a", Types.T_num); ("b", Types.T_num)] (user "Flag")
    |> add_fn "lt"      [("a", Types.T_num); ("b", Types.T_num)] (user "Flag")
    |> add_fn "gte"     [("a", Types.T_num); ("b", Types.T_num)] (user "Flag")
    |> add_fn "lte"     [("a", Types.T_num); ("b", Types.T_num)] (user "Flag")
    |> add_fn "eq_num"  [("a", Types.T_num); ("b", Types.T_num)] (user "Flag")
    |> add_fn "ne_num"  [("a", Types.T_num); ("b", Types.T_num)] (user "Flag")
    |> add_fn "between" [("lo", Types.T_num); ("x", Types.T_num); ("hi", Types.T_num)]
                        (user "Flag")
    |> add_fn "min"     [("a", Types.T_num); ("b", Types.T_num)] Types.T_num
    |> add_fn "max"     [("a", Types.T_num); ("b", Types.T_num)] Types.T_num
    |> add_fn "is_zero"     [("n", Types.T_num)] (user "Flag")
    |> add_fn "is_positive" [("n", Types.T_num)] (user "Flag")
    |> add_fn "is_negative" [("n", Types.T_num)] (user "Flag")

    (* §16.3 Flag combinators *)
    |> add_fn "flag_and"  [("a", user "Flag"); ("b", user "Flag")] (user "Flag")
    |> add_fn "flag_or"   [("a", user "Flag"); ("b", user "Flag")] (user "Flag")
    |> add_fn "flag_not"  [("a", user "Flag")] (user "Flag")
    |> add_fn "when_flag" [("cond", user "Flag"); ("then_", v 0); ("else_", v 0)] (v 0)

    (* §16.4 Result helpers *)
    |> add_fn "require" [("cond", user "Flag"); ("msg", Types.T_text)]
                        (result Types.T_unit Types.T_text)
    (* Unwrap a Result, fatal on Err. Useful when the ruleset has already
       proven the Result is Ok (e.g. just-refilled deck) and wants to
       skip the nested-match ceremony. *)
    |> add_fn "assume"  [("result", result (v 0) (v 1))] (v 0)

    (* §16.5 Cards. These all assume the common shape
         type Suit = Clubs | Diamonds | Hearts | Spades
         type Card = Card { suit: Suit, rank: Num }
       Games that declare an exotic Card simply don't call these. *)
    |> add_fn "fresh_deck"    [] (list (user "Card"))
    |> add_fn "card_rank"     [("card", user "Card")] Types.T_num
    |> add_fn "card_suit"     [("card", user "Card")] (user "Suit")
    |> add_fn "card_has_rank" [("card", user "Card"); ("rank", Types.T_num)] (user "Flag")
    |> add_fn "card_has_suit" [("card", user "Card"); ("suit", user "Suit")] (user "Flag")
    |> add_fn "cards_of_rank" [("cards", list (user "Card")); ("rank", Types.T_num)]
                              (list (user "Card"))
    |> add_fn "cards_of_suit" [("cards", list (user "Card")); ("suit", user "Suit")]
                              (list (user "Card"))

    (* §16.6 Visibility helper: SeeAll to owner, SeeSize to everyone else. *)
    |> add_fn "hand_visibility" [("owner", Types.T_player_id)]
                                (fn [Types.T_state; Types.T_player_id] (user "Visibility"))

    (* §16.7 Dealing helpers. `pile_of` is typically a parameterized pile
       constructor like `Hand`: the stdlib calls `pile_of(player)` for each
       player and deposits cards into the resulting PileRef<C>. *)
    |> add_fn "deal_evenly"
         [("state", Types.T_state); ("players", list Types.T_player_id);
          ("cards", list (v 0)); ("per", Types.T_num);
          ("pile_of", fn [Types.T_player_id] (pile_ref (v 0)))]
         Types.T_state
    |> add_fn "deal_cycle"
         [("state", Types.T_state); ("players", list Types.T_player_id);
          ("cards", list (v 0));
          ("pile_of", fn [Types.T_player_id] (pile_ref (v 0)))]
         Types.T_state

    (* §16.8 Deck refill: move `source` into `deck` and shuffle the deck.
       When `keep_top = On`, the top card of `source` stays put (the
       "Crazy Eights" pattern of preserving the discard top). *)
    |> add_fn "refill"
         [("state", Types.T_state); ("rng", Types.T_rng);
          ("deck", pile_ref (v 0)); ("source", pile_ref (v 0));
          ("keep_top", user "Flag")]
         Types.T_state
  in
  env
