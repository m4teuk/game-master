type key = Value.t list

type instance = {
  name : string;
  keys : key;
}

(* [Pile.t] is the state's pile registry: one entry per materialized
   [(name, keys)] tuple. Stored as a list — registries rarely exceed
   a few dozen entries in practice, so the linear scan is fine and
   structural sharing lets us return new [t]s cheaply. *)
type t = Value.pile_entry list

let empty : t = []

let keys_equal (ks1 : Value.t list) (ks2 : Value.t list) : bool =
  List.length ks1 = List.length ks2
  && List.for_all2 Value.equal ks1 ks2

let entry_matches (inst : instance) (e : Value.pile_entry) : bool =
  String.equal e.pe_name inst.name && keys_equal e.pe_keys inst.keys

(* ---------------------------------------------------------------- *)
(* Reads                                                              *)
(* ---------------------------------------------------------------- *)

let cards_in (t : t) (inst : instance) : Value.card list =
  match List.find_opt (entry_matches inst) t with
  | Some e -> e.pe_cards
  | None -> []

let size_of (t : t) (inst : instance) : int =
  List.length (cards_in t inst)

let is_materialized (t : t) (inst : instance) : bool =
  List.exists (entry_matches inst) t

let materialized_instances (t : t) : instance list =
  List.map (fun (e : Value.pile_entry) ->
    { name = e.pe_name; keys = e.pe_keys }) t

(* ---------------------------------------------------------------- *)
(* Internal: replace or insert an entry                               *)
(* ---------------------------------------------------------------- *)

let set_cards (t : t) (inst : instance)
    (new_cards : Value.card list) : t =
  let rec aux = function
    | [] ->
      [{ Value.pe_name = inst.name;
         pe_keys = inst.keys;
         pe_cards = new_cards }]
    | (e : Value.pile_entry) :: rest when entry_matches inst e ->
      { e with pe_cards = new_cards } :: rest
    | e :: rest -> e :: aux rest
  in
  aux t

let fatal_empty op (inst : instance) =
  raise (Value.Fatal
           (Printf.sprintf "%s: source pile '%s' is empty" op inst.name))

(* ---------------------------------------------------------------- *)
(* Mutations                                                          *)
(* ---------------------------------------------------------------- *)

let init_pile (t : t) (inst : instance) (cards : Value.card list) : t =
  (* Per stdlib §4: if the pile already exists, append cards to the top. *)
  let existing = cards_in t inst in
  set_cards t inst (cards @ existing)

let move_top (t : t) ~(from_ : instance) ~(to_ : instance) : t =
  match cards_in t from_ with
  | [] -> fatal_empty "move_top" from_
  | c :: rest ->
    let t = set_cards t from_ rest in
    let to_cards = cards_in t to_ in
    set_cards t to_ (c :: to_cards)

let move_card (t : t) ~(from_ : instance) ~(to_ : instance)
    (card : Value.card) : t =
  let from_cards = cards_in t from_ in
  let rec remove_first = function
    | [] ->
      raise (Value.Fatal
               (Printf.sprintf
                  "move_card: card not found in pile '%s'" from_.name))
    | c :: rest when Value.equal c card -> (c, rest)
    | c :: rest ->
      let (found, remaining) = remove_first rest in
      (found, c :: remaining)
  in
  let (c, rest) = remove_first from_cards in
  let t = set_cards t from_ rest in
  let to_cards = cards_in t to_ in
  set_cards t to_ (c :: to_cards)

let move_to_bottom (t : t) ~(from_ : instance) ~(to_ : instance) : t =
  match cards_in t from_ with
  | [] -> fatal_empty "move_to_bottom" from_
  | c :: rest ->
    let t = set_cards t from_ rest in
    let to_cards = cards_in t to_ in
    set_cards t to_ (to_cards @ [c])

let move_all (t : t) ~(from_ : instance) ~(to_ : instance) : t =
  let from_cards = cards_in t from_ in
  let t = set_cards t from_ [] in
  let to_cards = cards_in t to_ in
  set_cards t to_ (from_cards @ to_cards)

let move_all_to_bottom (t : t) ~(from_ : instance) ~(to_ : instance) : t =
  let from_cards = cards_in t from_ in
  let t = set_cards t from_ [] in
  let to_cards = cards_in t to_ in
  set_cards t to_ (to_cards @ from_cards)

let shuffle (t : t) (rng : Rng.t) (inst : instance) : t * Rng.t =
  let cards = cards_in t inst in
  let (shuffled, rng') = Rng.shuffle_list rng cards in
  (set_cards t inst shuffled, rng')

(* ---------------------------------------------------------------- *)
(* Temp-pile scope                                                    *)
(* ---------------------------------------------------------------- *)

(* Temp piles are identified by a name prefix the user can never emit
   (starts with ['$']). Each [apply] call opens a scope; [close_scope]
   strips all temp entries from the registry and errors if any were
   left non-empty. The scope's [counter] feeds a unique suffix so
   repeated [temp_pile()] calls produce distinct instances. *)

type scope = {
  mutable counter : int;
  scope_id : int;
}

let scope_id_counter = ref 0

let open_scope () =
  incr scope_id_counter;
  { counter = 0; scope_id = !scope_id_counter }

let is_temp (e : Value.pile_entry) : bool =
  String.length e.pe_name > 0 && Char.equal (String.get e.pe_name 0) '$'

let fresh_temp (scope : scope) : instance =
  scope.counter <- scope.counter + 1;
  let name = Printf.sprintf "$temp_%d_%d" scope.scope_id scope.counter in
  { name; keys = [] }

let close_scope (_scope : scope) (t : t) : (t, string) result =
  let leaked =
    List.filter_map (fun (e : Value.pile_entry) ->
      if is_temp e && e.pe_cards <> [] then Some e.pe_name else None) t
  in
  if leaked <> [] then
    Error (Printf.sprintf "temp pile(s) non-empty at apply end: %s"
             (String.concat ", " leaked))
  else
    Ok (List.filter (fun e -> not (is_temp e)) t)
