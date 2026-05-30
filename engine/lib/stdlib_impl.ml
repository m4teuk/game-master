(* Implementations of every builtin declared in stdlib.md.

   Each [impl] pattern-matches on the args it expects; typecheck is
   responsible for ensuring the shape is right, so any mismatch at
   runtime is an interpreter/typechecker bug and raises [Value.Fatal].

   Capability lists mirror the availability matrix in stdlib §17: a
   builtin's capabilities enumerate every context in which it is
   callable. Empty list = universal (pure functions, list ops,
   [fatal], etc.). *)

open Interp
open Value

(* ---------------------------------------------------------------- *)
(* Capability groups                                                  *)
(* ---------------------------------------------------------------- *)

(* State in scope: setup, apply, terminal, visibility. *)
let state_caps =
  [Cap_setup; Cap_apply; Cap_terminal; Cap_visibility]

(* View in scope: validate and the text-I/O callsites that receive a view. *)
let view_caps =
  [Cap_validate; Cap_text_to_action; Cap_view_to_text]

(* Both State and RNG: setup, apply. *)
let setup_apply_caps = [Cap_setup; Cap_apply]

(* ---------------------------------------------------------------- *)
(* Helpers                                                            *)
(* ---------------------------------------------------------------- *)

let type_err name args =
  raise (Value.Fatal
           (Printf.sprintf "%s: runtime type error (%d arg(s) of wrong shape)"
              name (List.length args)))

(* Find a pile-view entry in a view by (name, keys). *)
let find_view_pile (v : view) (inst_name : string) (inst_keys : t list)
    : view_pile option =
  List.find_opt (fun (vp : view_pile) ->
      String.equal vp.vp_name inst_name
      && List.length vp.vp_keys = List.length inst_keys
      && List.for_all2 Value.equal vp.vp_keys inst_keys)
    v.view_piles

(* Build a default [Value.t] for a given type, used by [new_state] to
   initialize per-player dicts (stdlib §13). Returns [None] for types
   that require explicit initialization (non-trivial user types). *)
let rec default_value_for (env : Types.env) (ty : Types.ty) : t option =
  match ty with
  | Types.T_num -> Some (V_num 0)
  | Types.T_text -> Some (V_text "")
  | Types.T_unit -> Some V_unit
  | Types.T_user name ->
    (match Types.lookup_type env name with
     | Some (Types.TI_adt { ctors = [c]; _ }) ->
       (* Single-ctor record: recurse on each field. *)
       let rec build_fields acc = function
         | [] -> Some (V_ctor { name = c.ctor_name; fields = List.rev acc })
         | (fn, fty) :: rest ->
           (match default_value_for env fty with
            | None -> None
            | Some v -> build_fields ((fn, v) :: acc) rest)
       in
       build_fields [] c.fields
     | _ -> None)
  | _ -> None

(* ---------------------------------------------------------------- *)
(* §1  Pile access — server-side                                      *)
(* ---------------------------------------------------------------- *)

let impl_cards_in (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref { name; keys }] ->
    let inst : Pile.instance = { name; keys } in
    V_list (Pile.cards_in s.state_piles inst)
  | _ -> type_err "cards_in" args

let impl_size_of (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref { name; keys }] ->
    let inst : Pile.instance = { name; keys } in
    V_num (Pile.size_of s.state_piles inst)
  | _ -> type_err "size_of" args

let impl_top_of (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref { name; keys }] ->
    let inst : Pile.instance = { name; keys } in
    (match Pile.cards_in s.state_piles inst with
     | c :: _ -> Value.ok c
     | [] -> Value.err (V_text "pile is empty"))
  | _ -> type_err "top_of" args

(* ---------------------------------------------------------------- *)
(* §2  Pile access — view-side (masked)                               *)
(* ---------------------------------------------------------------- *)

let impl_view_of (_ : ctx) args =
  match args with
  | [V_view v; V_pile_ref { name; keys }] ->
    (match find_view_pile v name keys with
     | Some vp -> vp.vp_value
     | None -> Value.masked  (* defensive: unmaterialized ⇒ Masked *))
  | _ -> type_err "view_of" args

let impl_visible_size_of (_ : ctx) args =
  match args with
  | [V_view v; V_pile_ref { name; keys }] ->
    (match find_view_pile v name keys with
     | None -> Value.err (V_text "pile not in view")
     | Some vp ->
       (match vp.vp_value with
        | V_ctor { name = "Contents"; fields = [("items", V_list xs)] } ->
          Value.ok (V_num (List.length xs))
        | V_ctor { name = "Size"; fields = [("n", V_num n)] } ->
          Value.ok (V_num n)
        | V_ctor { name = "Masked"; _ } ->
          Value.err (V_text "size not visible")
        | _ -> type_err "visible_size_of" args))
  | _ -> type_err "visible_size_of" args

let impl_visible_top_of (_ : ctx) args =
  match args with
  | [V_view v; V_pile_ref { name; keys }] ->
    (match find_view_pile v name keys with
     | None -> Value.err (V_text "pile not in view")
     | Some vp ->
       (match vp.vp_value with
        | V_ctor { name = "Contents"; fields = [("items", V_list (c :: _))] } ->
          Value.ok c
        | V_ctor { name = "Contents"; fields = [("items", V_list [])] } ->
          Value.err (V_text "pile is empty")
        | V_ctor { name = "Size"; _ } ->
          Value.err (V_text "contents not visible")
        | V_ctor { name = "Masked"; _ } ->
          Value.err (V_text "contents not visible")
        | _ -> type_err "visible_top_of" args))
  | _ -> type_err "visible_top_of" args

(* ---------------------------------------------------------------- *)
(* §3  Pile mutation                                                  *)
(* ---------------------------------------------------------------- *)

let instance_of (name : string) (keys : t list) : Pile.instance =
  { name; keys }

let impl_move_top (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref a; V_pile_ref b] ->
    let new_piles =
      Pile.move_top s.state_piles
        ~from_:(instance_of a.name a.keys)
        ~to_:(instance_of b.name b.keys)
    in
    V_state { s with state_piles = new_piles }
  | _ -> type_err "move_top" args

let impl_move_card (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref a; V_pile_ref b; card] ->
    let new_piles =
      Pile.move_card s.state_piles
        ~from_:(instance_of a.name a.keys)
        ~to_:(instance_of b.name b.keys)
        card
    in
    V_state { s with state_piles = new_piles }
  | _ -> type_err "move_card" args

let impl_move_to_bottom (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref a; V_pile_ref b] ->
    let new_piles =
      Pile.move_to_bottom s.state_piles
        ~from_:(instance_of a.name a.keys)
        ~to_:(instance_of b.name b.keys)
    in
    V_state { s with state_piles = new_piles }
  | _ -> type_err "move_to_bottom" args

let impl_move_all (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref a; V_pile_ref b] ->
    let new_piles =
      Pile.move_all s.state_piles
        ~from_:(instance_of a.name a.keys)
        ~to_:(instance_of b.name b.keys)
    in
    V_state { s with state_piles = new_piles }
  | _ -> type_err "move_all" args

let impl_move_all_to_bottom (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref a; V_pile_ref b] ->
    let new_piles =
      Pile.move_all_to_bottom s.state_piles
        ~from_:(instance_of a.name a.keys)
        ~to_:(instance_of b.name b.keys)
    in
    V_state { s with state_piles = new_piles }
  | _ -> type_err "move_all_to_bottom" args

let impl_shuffle ctx args =
  match args with
  | [V_state s; V_rng; V_pile_ref p] ->
    let rng_ref = ctx_rng ctx in
    let (new_piles, new_rng) =
      Pile.shuffle s.state_piles !rng_ref (instance_of p.name p.keys)
    in
    rng_ref := new_rng;
    V_state { s with state_piles = new_piles }
  | _ -> type_err "shuffle" args

(* ---------------------------------------------------------------- *)
(* §4  Setup-only                                                     *)
(* ---------------------------------------------------------------- *)

let impl_new_state ctx args =
  match args with
  | [config] ->
    let link = ctx_link ctx in
    let roster = ctx_roster ctx in
    let pd_type_name = link.Link.player_dict_type in
    let pd_default =
      match default_value_for link.env (Types.T_user pd_type_name) with
      | Some v -> v
      | None ->
        raise (Value.Fatal
                 (Printf.sprintf
                    "new_state: PlayerDict type '%s' contains a field with \
                     no default; initialize via update_player_dict in setup"
                    pd_type_name))
    in
    let dicts = List.map (fun p -> (p, pd_default)) roster in
    V_state {
      state_config = config;
      state_player_dicts = dicts;
      state_piles = Pile.empty;
      state_roster = roster;
    }
  | _ -> type_err "new_state" args

let impl_init_pile (_ : ctx) args =
  match args with
  | [V_state s; V_pile_ref p; V_list cards] ->
    let new_piles =
      Pile.init_pile s.state_piles (instance_of p.name p.keys) cards
    in
    V_state { s with state_piles = new_piles }
  | _ -> type_err "init_pile" args

(* ---------------------------------------------------------------- *)
(* §5  Apply-only                                                     *)
(* ---------------------------------------------------------------- *)

let impl_temp_pile ctx args =
  match args with
  | [] ->
    (match ctx_temp_scope ctx with
     | None ->
       raise (Value.Fatal
                "temp_pile: no temp scope (only valid inside apply)")
     | Some scope ->
       let inst = Pile.fresh_temp scope in
       V_pile_ref { name = inst.name; keys = inst.keys })
  | _ -> type_err "temp_pile" args

(* ---------------------------------------------------------------- *)
(* §6  Config, player dict, roster                                    *)
(* ---------------------------------------------------------------- *)

let impl_config_of (_ : ctx) args =
  match args with
  | [V_state s] -> s.state_config
  | _ -> type_err "config_of" args

let impl_view_config (_ : ctx) args =
  match args with
  | [V_view v] -> v.view_config
  | _ -> type_err "view_config" args

let impl_with_config (_ : ctx) args =
  match args with
  | [V_state s; cfg] -> V_state { s with state_config = cfg }
  | _ -> type_err "with_config" args

let impl_player_dict (_ : ctx) args =
  match args with
  | [V_state s; V_player p] ->
    (match List.assoc_opt p s.state_player_dicts with
     | Some d -> d
     | None ->
       raise (Value.Fatal
                (Printf.sprintf "player_dict: '%s' not in roster" p)))
  | _ -> type_err "player_dict" args

let impl_update_player_dict ctx args =
  match args with
  | [V_state s; V_player p; f] ->
    (match List.assoc_opt p s.state_player_dicts with
     | None ->
       raise (Value.Fatal
                (Printf.sprintf "update_player_dict: '%s' not in roster" p))
     | Some d ->
       let new_d = Interp.call ctx f [d] in
       let new_dicts = List.map (fun (q, old) ->
         if String.equal q p then (q, new_d) else (q, old))
           s.state_player_dicts
       in
       V_state { s with state_player_dicts = new_dicts })
  | _ -> type_err "update_player_dict" args

let impl_players_of (_ : ctx) args =
  match args with
  | [V_state s] ->
    V_list (List.map (fun p -> V_player p) s.state_roster)
  | _ -> type_err "players_of" args

let impl_players_of_view (_ : ctx) args =
  match args with
  | [V_view v] ->
    V_list (List.map (fun p -> V_player p) v.view_roster)
  | _ -> type_err "players_of_view" args

(* ---------------------------------------------------------------- *)
(* §7  RNG                                                            *)
(* ---------------------------------------------------------------- *)

let impl_random_int ctx args =
  match args with
  | [V_rng; V_num lo; V_num hi] ->
    if lo > hi then
      raise (Value.Fatal
               (Printf.sprintf "random_int: lo (%d) > hi (%d)" lo hi));
    let rng_ref = ctx_rng ctx in
    let (n, new_rng) = Rng.random_int !rng_ref ~lo ~hi in
    rng_ref := new_rng;
    V_num n
  | _ -> type_err "random_int" args

let impl_shuffle_list ctx args =
  match args with
  | [V_rng; V_list xs] ->
    let rng_ref = ctx_rng ctx in
    let (shuffled, new_rng) = Rng.shuffle_list !rng_ref xs in
    rng_ref := new_rng;
    V_list shuffled
  | _ -> type_err "shuffle_list" args

(* ---------------------------------------------------------------- *)
(* §8  Visibility helpers                                             *)
(* ---------------------------------------------------------------- *)

(* [public], [public_size], [hidden] are (State, PlayerId) -> Visibility
   functions called at view-construction time. They ignore both args
   and return a constant Visibility. *)

let impl_public (_ : ctx) args =
  match args with
  | [V_state _; V_player _] -> Value.see_all
  | _ -> type_err "public" args

let impl_public_size (_ : ctx) args =
  match args with
  | [V_state _; V_player _] -> Value.see_size
  | _ -> type_err "public_size" args

let impl_hidden_ (_ : ctx) args =
  match args with
  | [V_state _; V_player _] -> Value.hidden
  | _ -> type_err "hidden" args

let impl_owner_only (_ : ctx) args =
  match args with
  | [V_player owner] ->
    V_partial {
      arity = 2;
      impl = (fun inner_args ->
        match inner_args with
        | [V_state _; V_player viewer] ->
          if String.equal viewer owner then Value.see_all else Value.hidden
        | _ ->
          raise (Value.Fatal "owner_only(_): inner application type error"));
    }
  | _ -> type_err "owner_only" args

(* ---------------------------------------------------------------- *)
(* §9  Lists                                                          *)
(* ---------------------------------------------------------------- *)

let impl_length (_ : ctx) args =
  match args with
  | [V_list xs] -> V_num (List.length xs)
  | _ -> type_err "length" args

let impl_map ctx args =
  match args with
  | [V_list xs; f] -> V_list (List.map (fun x -> Interp.call ctx f [x]) xs)
  | _ -> type_err "map" args

let flag_is_on = function
  | V_ctor { name = "On"; _ } -> true
  | _ -> false

let impl_filter ctx args =
  match args with
  | [V_list xs; f] ->
    V_list (List.filter (fun x -> flag_is_on (Interp.call ctx f [x])) xs)
  | _ -> type_err "filter" args

let impl_fold ctx args =
  match args with
  | [V_list xs; init; f] ->
    List.fold_left (fun acc x -> Interp.call ctx f [acc; x]) init xs
  | _ -> type_err "fold" args

let impl_flatmap ctx args =
  match args with
  | [V_list xs; f] ->
    V_list (List.concat_map (fun x ->
      match Interp.call ctx f [x] with
      | V_list ys -> ys
      | _ -> raise (Value.Fatal "flatmap: callback did not return a list"))
        xs)
  | _ -> type_err "flatmap" args

let impl_append (_ : ctx) args =
  match args with
  | [V_list xs; V_list ys] -> V_list (xs @ ys)
  | _ -> type_err "append" args

let impl_nth (_ : ctx) args =
  match args with
  | [V_list xs; V_num n] ->
    if n < 0 || n >= List.length xs then
      Value.err (V_text (Printf.sprintf "nth: index %d out of range" n))
    else
      Value.ok (List.nth xs n)
  | _ -> type_err "nth" args

let impl_member (_ : ctx) args =
  match args with
  | [V_list xs; needle] ->
    if List.exists (Value.equal needle) xs then Value.flag_on
    else Value.flag_off
  | _ -> type_err "member" args

let impl_any ctx args =
  match args with
  | [V_list xs; f] ->
    if List.exists (fun x -> flag_is_on (Interp.call ctx f [x])) xs
    then Value.flag_on else Value.flag_off
  | _ -> type_err "any" args

let impl_all ctx args =
  match args with
  | [V_list xs; f] ->
    if List.for_all (fun x -> flag_is_on (Interp.call ctx f [x])) xs
    then Value.flag_on else Value.flag_off
  | _ -> type_err "all" args

let impl_split_at (_ : ctx) args =
  match args with
  | [V_list xs; V_num n] ->
    if n < 0 then raise (Value.Fatal "split_at: n < 0");
    let rec split k acc = function
      | xs when k <= 0 -> (List.rev acc, xs)
      | [] -> (List.rev acc, [])
      | x :: rest -> split (k - 1) (x :: acc) rest
    in
    let (a, b) = split n [] xs in
    V_tuple [V_list a; V_list b]
  | _ -> type_err "split_at" args

let impl_next_in_cycle (_ : ctx) args =
  match args with
  | [V_list xs; needle] ->
    let rec find = function
      | [] ->
        raise (Value.Fatal "next_in_cycle: element not in list")
      | x :: rest when Value.equal x needle ->
        (match rest with
         | y :: _ -> y
         | [] ->
           (match xs with
            | y :: _ -> y
            | [] ->
              raise (Value.Fatal
                       "next_in_cycle: element not in empty list")))
      | _ :: rest -> find rest
    in
    find xs
  | _ -> type_err "next_in_cycle" args

(* ---------------------------------------------------------------- *)
(* §10  Result                                                        *)
(* ---------------------------------------------------------------- *)

let impl_ok (_ : ctx) args =
  match args with
  | [v] -> Value.ok v
  | _ -> type_err "ok" args

let impl_err (_ : ctx) args =
  match args with
  | [v] -> Value.err v
  | _ -> type_err "err" args

let impl_and_then ctx args =
  match args with
  | [V_ctor { name = "Ok"; fields = [("value", v)] }; f] ->
    Interp.call ctx f [v]
  | [V_ctor { name = "Err"; _ } as e; _] -> e
  | _ -> type_err "and_then" args

(* ---------------------------------------------------------------- *)
(* §11  Comparison and branching                                      *)
(* ---------------------------------------------------------------- *)

let impl_compare (_ : ctx) args =
  match args with
  | [V_num a; V_num b] ->
    if a < b then Value.lt
    else if a = b then Value.eq_ord
    else Value.gt
  | _ -> type_err "compare" args

let impl_eq (_ : ctx) args =
  match args with
  | [a; b] ->
    if Value.equal a b then Value.flag_on else Value.flag_off
  | _ -> type_err "eq" args

let impl_if_eq (_ : ctx) args =
  match args with
  | [a; b; t; e] -> if Value.equal a b then t else e
  | _ -> type_err "if_eq" args

(* ---------------------------------------------------------------- *)
(* §12  Fatal                                                         *)
(* ---------------------------------------------------------------- *)

let impl_fatal (_ : ctx) args =
  match args with
  | [V_text msg] -> raise (Value.Fatal msg)
  | _ -> type_err "fatal" args

(* ---------------------------------------------------------------- *)
(* §14  Text and JSON rendering builtins                              *)
(* ---------------------------------------------------------------- *)

let impl_builtin_action_to_text ctx args =
  match args with
  | [action; V_player p] ->
    V_text (Render.action_to_text (ctx_link ctx) action p)
  | _ -> type_err "builtin_action_to_text" args

let impl_builtin_text_to_action ctx args =
  match args with
  | [V_text t; V_view v; V_player p] ->
    (match Render.text_to_action (ctx_link ctx) t ~view:v ~player:p with
     | Ok a -> Value.ok a
     | Error e -> Value.err (V_text e))
  | _ -> type_err "builtin_text_to_action" args

let impl_builtin_view_to_text ctx args =
  match args with
  | [V_view v; V_player p] ->
    V_text (Render.view_to_text (ctx_link ctx) v p)
  | _ -> type_err "builtin_view_to_text" args

let impl_builtin_outcome_to_text ctx args =
  match args with
  | [outcome; V_player p] ->
    V_text (Render.outcome_to_text (ctx_link ctx) outcome p)
  | _ -> type_err "builtin_outcome_to_text" args

let impl_builtin_action_to_json ctx args =
  match args with
  | [action; V_player p] ->
    V_text (Render.action_to_json (ctx_link ctx) action p)
  | _ -> type_err "builtin_action_to_json" args

let impl_builtin_json_to_action ctx args =
  match args with
  | [V_text t; V_view v; V_player p] ->
    (match Render.json_to_action (ctx_link ctx) t ~view:v ~player:p with
     | Ok a -> Value.ok a
     | Error e -> Value.err (V_text e))
  | _ -> type_err "builtin_json_to_action" args

let impl_builtin_view_to_json ctx args =
  match args with
  | [V_view v; V_player p] ->
    V_text (Render.view_to_json (ctx_link ctx) v p)
  | _ -> type_err "builtin_view_to_json" args

let impl_builtin_outcome_to_json ctx args =
  match args with
  | [outcome; V_player p] ->
    V_text (Render.outcome_to_json (ctx_link ctx) outcome p)
  | _ -> type_err "builtin_outcome_to_json" args

(* ---------------------------------------------------------------- *)
(* §15  PlayerId conversion                                           *)
(* ---------------------------------------------------------------- *)

let impl_player_id_to_text (_ : ctx) args =
  match args with
  | [V_player p] -> V_text p
  | _ -> type_err "player_id_to_text" args

let impl_text_to_player_id (_ : ctx) args =
  match args with
  | [V_text s; V_view v] ->
    if List.mem s v.view_roster then Value.ok (V_player s)
    else Value.err (V_text (Printf.sprintf "no such player '%s'" s))
  | _ -> type_err "text_to_player_id" args

(* ---------------------------------------------------------------- *)
(* §16  Extended stdlib                                               *)
(* ---------------------------------------------------------------- *)

(* §16.1  List ops *)

let impl_range (_ : ctx) args =
  match args with
  | [V_num lo; V_num hi] ->
    let rec go i acc =
      if i > hi then List.rev acc else go (i + 1) (V_num i :: acc)
    in
    V_list (go lo [])
  | _ -> type_err "range" args

let take_list (xs : Value.t list) (n : int) : Value.t list =
  if n <= 0 then []
  else
    let rec go k xs acc =
      if k <= 0 then List.rev acc
      else match xs with
        | [] -> List.rev acc
        | x :: r -> go (k - 1) r (x :: acc)
    in
    go n xs []

let drop_list (xs : Value.t list) (n : int) : Value.t list =
  if n <= 0 then xs
  else
    let rec go k xs =
      if k <= 0 then xs
      else match xs with
        | [] -> []
        | _ :: r -> go (k - 1) r
    in
    go n xs

let impl_take (_ : ctx) args =
  match args with
  | [V_list xs; V_num n] ->
    if n < 0 then raise (Value.Fatal "take: n < 0");
    V_list (take_list xs n)
  | _ -> type_err "take" args

let impl_drop (_ : ctx) args =
  match args with
  | [V_list xs; V_num n] ->
    if n < 0 then raise (Value.Fatal "drop: n < 0");
    V_list (drop_list xs n)
  | _ -> type_err "drop" args

let impl_count ctx args =
  match args with
  | [V_list xs; f] ->
    let n =
      List.fold_left (fun acc x ->
        if flag_is_on (Interp.call ctx f [x]) then acc + 1 else acc)
        0 xs
    in
    V_num n
  | _ -> type_err "count" args

let impl_find ctx args =
  match args with
  | [V_list xs; f] ->
    (match List.find_opt
             (fun x -> flag_is_on (Interp.call ctx f [x])) xs with
     | Some x -> Value.ok x
     | None   -> Value.err (V_text "find: no matching element"))
  | _ -> type_err "find" args

let impl_concat (_ : ctx) args =
  match args with
  | [V_list lol] ->
    V_list (List.concat_map (fun l ->
      match l with
      | V_list xs -> xs
      | _ -> raise (Value.Fatal "concat: expected list of lists")) lol)
  | _ -> type_err "concat" args

let impl_reverse (_ : ctx) args =
  match args with
  | [V_list xs] -> V_list (List.rev xs)
  | _ -> type_err "reverse" args

let impl_repeat (_ : ctx) args =
  match args with
  | [V_num n; x] ->
    if n < 0 then raise (Value.Fatal "repeat: n < 0");
    let rec go k acc = if k <= 0 then acc else go (k - 1) (x :: acc) in
    V_list (go n [])
  | _ -> type_err "repeat" args

let impl_zip (_ : ctx) args =
  match args with
  | [V_list xs; V_list ys] ->
    let rec go xs ys acc =
      match xs, ys with
      | [], _ | _, [] -> List.rev acc
      | x :: rx, y :: ry -> go rx ry (V_tuple [x; y] :: acc)
    in
    V_list (go xs ys [])
  | _ -> type_err "zip" args

let impl_sum (_ : ctx) args =
  match args with
  | [V_list xs] ->
    let n = List.fold_left (fun acc v ->
      match v with
      | V_num n -> acc + n
      | _ -> raise (Value.Fatal "sum: list contains a non-Num")) 0 xs
    in
    V_num n
  | _ -> type_err "sum" args

let impl_is_empty (_ : ctx) args =
  match args with
  | [V_list []] -> Value.flag_on
  | [V_list _]  -> Value.flag_off
  | _ -> type_err "is_empty" args

let impl_head (_ : ctx) args =
  match args with
  | [V_list (x :: _)] -> Value.ok x
  | [V_list []]       -> Value.err (V_text "head: empty list")
  | _ -> type_err "head" args

let impl_tail (_ : ctx) args =
  match args with
  | [V_list (_ :: xs)] -> Value.ok (V_list xs)
  | [V_list []]        -> Value.err (V_text "tail: empty list")
  | _ -> type_err "tail" args

(* §16.2  Numeric predicates and helpers *)

let num_pred (pred : int -> int -> bool) : ctx -> Value.t list -> Value.t =
  fun _ args ->
    match args with
    | [V_num a; V_num b] ->
      if pred a b then Value.flag_on else Value.flag_off
    | _ -> raise (Value.Fatal "numeric predicate: type error")

let impl_gt     = num_pred ( > )
let impl_lt     = num_pred ( < )
let impl_gte    = num_pred ( >= )
let impl_lte    = num_pred ( <= )
let impl_eq_num = num_pred ( = )
let impl_ne_num = num_pred ( <> )

let impl_between (_ : ctx) args =
  match args with
  | [V_num lo; V_num x; V_num hi] ->
    if lo <= x && x <= hi then Value.flag_on else Value.flag_off
  | _ -> type_err "between" args

let impl_min (_ : ctx) args =
  match args with
  | [V_num a; V_num b] -> V_num (if a < b then a else b)
  | _ -> type_err "min" args

let impl_max (_ : ctx) args =
  match args with
  | [V_num a; V_num b] -> V_num (if a > b then a else b)
  | _ -> type_err "max" args

let impl_is_zero (_ : ctx) args =
  match args with
  | [V_num 0] -> Value.flag_on
  | [V_num _] -> Value.flag_off
  | _ -> type_err "is_zero" args

let impl_is_positive (_ : ctx) args =
  match args with
  | [V_num n] -> if n > 0 then Value.flag_on else Value.flag_off
  | _ -> type_err "is_positive" args

let impl_is_negative (_ : ctx) args =
  match args with
  | [V_num n] -> if n < 0 then Value.flag_on else Value.flag_off
  | _ -> type_err "is_negative" args

(* §16.3  Flag combinators *)

let impl_flag_and (_ : ctx) args =
  match args with
  | [a; b] ->
    if flag_is_on a && flag_is_on b then Value.flag_on else Value.flag_off
  | _ -> type_err "flag_and" args

let impl_flag_or (_ : ctx) args =
  match args with
  | [a; b] ->
    if flag_is_on a || flag_is_on b then Value.flag_on else Value.flag_off
  | _ -> type_err "flag_or" args

let impl_flag_not (_ : ctx) args =
  match args with
  | [a] -> if flag_is_on a then Value.flag_off else Value.flag_on
  | _ -> type_err "flag_not" args

let impl_when_flag (_ : ctx) args =
  match args with
  | [cond; t; e] -> if flag_is_on cond then t else e
  | _ -> type_err "when_flag" args

(* §16.4  Result helpers *)

let impl_require (_ : ctx) args =
  match args with
  | [cond; V_text msg] ->
    if flag_is_on cond then Value.ok V_unit else Value.err (V_text msg)
  | _ -> type_err "require" args

let impl_assume (_ : ctx) args =
  match args with
  | [V_ctor { name = "Ok";  fields = [("value", v)] }] -> v
  | [V_ctor { name = "Err"; fields = [("error", V_text msg)] }] ->
    raise (Value.Fatal ("assume: Err(" ^ msg ^ ")"))
  | [V_ctor { name = "Err"; _ }] ->
    raise (Value.Fatal "assume: Err")
  | _ -> type_err "assume" args

(* §16.5  Cards. Assume shape Card { suit: Suit, rank: Num } with standard
   Suit = Clubs | Diamonds | Hearts | Spades. *)

let standard_suits = ["Clubs"; "Diamonds"; "Hearts"; "Spades"]
let standard_ranks = [2;3;4;5;6;7;8;9;10;11;12;13;14]

let make_card suit_name rank : Value.t =
  V_ctor {
    name = "Card";
    fields = [
      ("suit", V_ctor { name = suit_name; fields = [] });
      ("rank", V_num rank);
    ];
  }

let impl_fresh_deck (_ : ctx) args =
  match args with
  | [] ->
    V_list (List.concat_map (fun s ->
      List.map (fun r -> make_card s r) standard_ranks) standard_suits)
  | _ -> type_err "fresh_deck" args

let card_field (c : Value.t) (field : string) : Value.t =
  match c with
  | V_ctor { fields; _ } ->
    (match List.assoc_opt field fields with
     | Some v -> v
     | None ->
       raise (Value.Fatal
                (Printf.sprintf
                   "card_*: Card value has no '%s' field (stdlib §16 \
                    assumes Card { suit: Suit, rank: Num })" field)))
  | _ ->
    raise (Value.Fatal "card_*: expected a Card value")

let impl_card_rank (_ : ctx) args =
  match args with
  | [c] -> card_field c "rank"
  | _ -> type_err "card_rank" args

let impl_card_suit (_ : ctx) args =
  match args with
  | [c] -> card_field c "suit"
  | _ -> type_err "card_suit" args

let impl_card_has_rank (_ : ctx) args =
  match args with
  | [c; V_num r] ->
    (match card_field c "rank" with
     | V_num n when n = r -> Value.flag_on
     | V_num _ -> Value.flag_off
     | _ -> raise (Value.Fatal "card_has_rank: Card.rank is not a Num"))
  | _ -> type_err "card_has_rank" args

let impl_card_has_suit (_ : ctx) args =
  match args with
  | [c; suit] ->
    if Value.equal (card_field c "suit") suit
    then Value.flag_on else Value.flag_off
  | _ -> type_err "card_has_suit" args

let impl_cards_of_rank (_ : ctx) args =
  match args with
  | [V_list xs; V_num r] ->
    V_list (List.filter (fun c ->
      match card_field c "rank" with V_num n -> n = r | _ -> false) xs)
  | _ -> type_err "cards_of_rank" args

let impl_cards_of_suit (_ : ctx) args =
  match args with
  | [V_list xs; suit] ->
    V_list (List.filter (fun c ->
      Value.equal (card_field c "suit") suit) xs)
  | _ -> type_err "cards_of_suit" args

(* §16.6  Visibility helper *)

let impl_hand_visibility (_ : ctx) args =
  match args with
  | [V_player owner] ->
    V_partial {
      arity = 2;
      impl = (fun inner_args ->
        match inner_args with
        | [V_state _; V_player viewer] ->
          if String.equal viewer owner
          then Value.see_all else Value.see_size
        | _ ->
          raise (Value.Fatal
                   "hand_visibility(_): inner application type error"));
    }
  | _ -> type_err "hand_visibility" args

(* §16.7  Dealing helpers *)

let pile_instance_of_value (v : Value.t) : Pile.instance =
  match v with
  | V_pile_ref { name; keys } -> { name; keys }
  | _ -> raise (Value.Fatal
                  "deal_*/refill: expected a PileRef (got a non-pile value)")

let impl_deal_evenly ctx args =
  match args with
  | [V_state s; V_list players; V_list cards; V_num per; pile_fn] ->
    if per < 0 then raise (Value.Fatal "deal_evenly: per < 0");
    let rec loop (piles : Pile.t) (remaining : Value.t list)
        (players_left : Value.t list) : Pile.t =
      match players_left with
      | [] -> piles
      | p :: rest ->
        let take_now = take_list remaining per in
        let more = drop_list remaining per in
        let pile_ref = Interp.call ctx pile_fn [p] in
        let inst = pile_instance_of_value pile_ref in
        let piles = Pile.init_pile piles inst take_now in
        loop piles more rest
    in
    V_state { s with state_piles = loop s.state_piles cards players }
  | _ -> type_err "deal_evenly" args

let impl_deal_cycle ctx args =
  match args with
  | [V_state s; V_list players; V_list cards; pile_fn] ->
    if players = [] && cards <> [] then
      raise (Value.Fatal "deal_cycle: cannot deal to an empty player list");
    let rec loop (piles : Pile.t) (remaining : Value.t list)
        (cycle : Value.t list) : Pile.t =
      match remaining with
      | [] -> piles
      | c :: rest ->
        (match cycle with
         | [] -> loop piles remaining players
         | p :: more ->
           let pile_ref = Interp.call ctx pile_fn [p] in
           let inst = pile_instance_of_value pile_ref in
           let piles = Pile.init_pile piles inst [c] in
           loop piles rest more)
    in
    V_state { s with state_piles = loop s.state_piles cards players }
  | _ -> type_err "deal_cycle" args

(* §16.8  Deck refill *)

let impl_refill ctx args =
  match args with
  | [V_state s; V_rng;
     V_pile_ref { name = d_n; keys = d_k };
     V_pile_ref { name = s_n; keys = s_k };
     keep_top] ->
    let deck_inst : Pile.instance = { name = d_n; keys = d_k } in
    let source_inst : Pile.instance = { name = s_n; keys = s_k } in
    let source_cards = Pile.cards_in s.state_piles source_inst in
    let to_move =
      if flag_is_on keep_top then
        match source_cards with
        | [] | [_] -> []
        | _ :: rest -> rest
      else source_cards
    in
    (* Nothing to move = no-op. Avoids a spurious shuffle of the deck
       when the source was empty (or only the preserved top). *)
    if to_move = [] then V_state s
    else begin
      let piles =
        List.fold_left (fun p c ->
          Pile.move_card p ~from_:source_inst ~to_:deck_inst c)
          s.state_piles to_move
      in
      let rng_ref = ctx_rng ctx in
      let (piles, new_rng) = Pile.shuffle piles !rng_ref deck_inst in
      rng_ref := new_rng;
      V_state { s with state_piles = piles }
    end
  | _ -> type_err "refill" args

(* ---------------------------------------------------------------- *)
(* Dispatch table                                                     *)
(* ---------------------------------------------------------------- *)

let all : builtin list = [
  (* §1 *)
  { name = "cards_in";             capabilities = state_caps;        impl = impl_cards_in };
  { name = "size_of";              capabilities = state_caps;        impl = impl_size_of };
  { name = "top_of";               capabilities = state_caps;        impl = impl_top_of };

  (* §2 *)
  { name = "view_of";              capabilities = view_caps;         impl = impl_view_of };
  { name = "visible_size_of";      capabilities = view_caps;         impl = impl_visible_size_of };
  { name = "visible_top_of";       capabilities = view_caps;         impl = impl_visible_top_of };

  (* §3 *)
  { name = "move_top";             capabilities = setup_apply_caps;  impl = impl_move_top };
  { name = "move_card";            capabilities = setup_apply_caps;  impl = impl_move_card };
  { name = "move_to_bottom";       capabilities = setup_apply_caps;  impl = impl_move_to_bottom };
  { name = "move_all";             capabilities = setup_apply_caps;  impl = impl_move_all };
  { name = "move_all_to_bottom";   capabilities = setup_apply_caps;  impl = impl_move_all_to_bottom };
  { name = "shuffle";              capabilities = setup_apply_caps;  impl = impl_shuffle };

  (* §4 *)
  { name = "new_state";            capabilities = [Cap_setup];       impl = impl_new_state };
  { name = "init_pile";            capabilities = [Cap_setup];       impl = impl_init_pile };

  (* §5 *)
  { name = "temp_pile";            capabilities = [Cap_apply];       impl = impl_temp_pile };

  (* §6 *)
  { name = "config_of";            capabilities = state_caps;        impl = impl_config_of };
  { name = "view_config";          capabilities = view_caps;         impl = impl_view_config };
  { name = "with_config";          capabilities = setup_apply_caps;  impl = impl_with_config };
  { name = "player_dict";          capabilities = state_caps;        impl = impl_player_dict };
  { name = "update_player_dict";   capabilities = setup_apply_caps;  impl = impl_update_player_dict };
  { name = "players_of";           capabilities = state_caps;        impl = impl_players_of };
  { name = "players_of_view";      capabilities = view_caps;         impl = impl_players_of_view };

  (* §7 *)
  { name = "random_int";           capabilities = setup_apply_caps;  impl = impl_random_int };
  { name = "shuffle_list";         capabilities = setup_apply_caps;  impl = impl_shuffle_list };

  (* §8 — all visibility helpers go through Cap_visibility at evaluation time *)
  { name = "public";               capabilities = [Cap_visibility];  impl = impl_public };
  { name = "public_size";          capabilities = [Cap_visibility];  impl = impl_public_size };
  { name = "hidden";               capabilities = [Cap_visibility];  impl = impl_hidden_ };
  { name = "owner_only";           capabilities = [];                impl = impl_owner_only };

  (* §9  List ops — pure, universal *)
  { name = "length";               capabilities = []; impl = impl_length };
  { name = "map";                  capabilities = []; impl = impl_map };
  { name = "filter";               capabilities = []; impl = impl_filter };
  { name = "fold";                 capabilities = []; impl = impl_fold };
  { name = "flatmap";              capabilities = []; impl = impl_flatmap };
  { name = "append";               capabilities = []; impl = impl_append };
  { name = "nth";                  capabilities = []; impl = impl_nth };
  { name = "member";               capabilities = []; impl = impl_member };
  { name = "any";                  capabilities = []; impl = impl_any };
  { name = "all";                  capabilities = []; impl = impl_all };
  { name = "split_at";             capabilities = []; impl = impl_split_at };
  { name = "next_in_cycle";        capabilities = []; impl = impl_next_in_cycle };

  (* §10 *)
  { name = "ok";                   capabilities = []; impl = impl_ok };
  { name = "err";                  capabilities = []; impl = impl_err };
  { name = "and_then";             capabilities = []; impl = impl_and_then };

  (* §11 *)
  { name = "compare";              capabilities = []; impl = impl_compare };
  { name = "eq";                   capabilities = []; impl = impl_eq };
  { name = "if_eq";                capabilities = []; impl = impl_if_eq };

  (* §12 *)
  { name = "fatal";                capabilities = []; impl = impl_fatal };

  (* §14 *)
  { name = "builtin_action_to_text";  capabilities = []; impl = impl_builtin_action_to_text };
  { name = "builtin_text_to_action";  capabilities = []; impl = impl_builtin_text_to_action };
  { name = "builtin_view_to_text";    capabilities = []; impl = impl_builtin_view_to_text };
  { name = "builtin_outcome_to_text"; capabilities = []; impl = impl_builtin_outcome_to_text };
  { name = "builtin_action_to_json";  capabilities = []; impl = impl_builtin_action_to_json };
  { name = "builtin_json_to_action";  capabilities = []; impl = impl_builtin_json_to_action };
  { name = "builtin_view_to_json";    capabilities = []; impl = impl_builtin_view_to_json };
  { name = "builtin_outcome_to_json"; capabilities = []; impl = impl_builtin_outcome_to_json };

  (* §15 *)
  { name = "player_id_to_text";    capabilities = []; impl = impl_player_id_to_text };
  { name = "text_to_player_id";    capabilities = view_caps; impl = impl_text_to_player_id };

  (* §16.1  Extended list ops — pure *)
  { name = "range";                capabilities = []; impl = impl_range };
  { name = "take";                 capabilities = []; impl = impl_take };
  { name = "drop";                 capabilities = []; impl = impl_drop };
  { name = "count";                capabilities = []; impl = impl_count };
  { name = "find";                 capabilities = []; impl = impl_find };
  { name = "concat";               capabilities = []; impl = impl_concat };
  { name = "reverse";              capabilities = []; impl = impl_reverse };
  { name = "repeat";               capabilities = []; impl = impl_repeat };
  { name = "zip";                  capabilities = []; impl = impl_zip };
  { name = "sum";                  capabilities = []; impl = impl_sum };
  { name = "is_empty";             capabilities = []; impl = impl_is_empty };
  { name = "head";                 capabilities = []; impl = impl_head };
  { name = "tail";                 capabilities = []; impl = impl_tail };

  (* §16.2  Numeric predicates *)
  { name = "gt";                   capabilities = []; impl = impl_gt };
  { name = "lt";                   capabilities = []; impl = impl_lt };
  { name = "gte";                  capabilities = []; impl = impl_gte };
  { name = "lte";                  capabilities = []; impl = impl_lte };
  { name = "eq_num";               capabilities = []; impl = impl_eq_num };
  { name = "ne_num";               capabilities = []; impl = impl_ne_num };
  { name = "between";              capabilities = []; impl = impl_between };
  { name = "min";                  capabilities = []; impl = impl_min };
  { name = "max";                  capabilities = []; impl = impl_max };
  { name = "is_zero";              capabilities = []; impl = impl_is_zero };
  { name = "is_positive";          capabilities = []; impl = impl_is_positive };
  { name = "is_negative";          capabilities = []; impl = impl_is_negative };

  (* §16.3  Flag combinators *)
  { name = "flag_and";             capabilities = []; impl = impl_flag_and };
  { name = "flag_or";              capabilities = []; impl = impl_flag_or };
  { name = "flag_not";             capabilities = []; impl = impl_flag_not };
  { name = "when_flag";            capabilities = []; impl = impl_when_flag };

  (* §16.4  Result helpers *)
  { name = "require";              capabilities = []; impl = impl_require };
  { name = "assume";               capabilities = []; impl = impl_assume };

  (* §16.5  Cards *)
  { name = "fresh_deck";           capabilities = []; impl = impl_fresh_deck };
  { name = "card_rank";            capabilities = []; impl = impl_card_rank };
  { name = "card_suit";            capabilities = []; impl = impl_card_suit };
  { name = "card_has_rank";        capabilities = []; impl = impl_card_has_rank };
  { name = "card_has_suit";        capabilities = []; impl = impl_card_has_suit };
  { name = "cards_of_rank";        capabilities = []; impl = impl_cards_of_rank };
  { name = "cards_of_suit";        capabilities = []; impl = impl_cards_of_suit };

  (* §16.6  Visibility helper — evaluated in Cap_visibility like its peers *)
  { name = "hand_visibility";      capabilities = []; impl = impl_hand_visibility };

  (* §16.7  Dealing helpers — setup and apply both manipulate piles *)
  { name = "deal_evenly";          capabilities = setup_apply_caps;  impl = impl_deal_evenly };
  { name = "deal_cycle";           capabilities = setup_apply_caps;  impl = impl_deal_cycle };

  (* §16.8  Refill *)
  { name = "refill";               capabilities = setup_apply_caps;  impl = impl_refill };
]

let lookup name =
  List.find_opt (fun (b : builtin) -> String.equal b.name name) all
