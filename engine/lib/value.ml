type player_id = string

exception Fatal of string

type t =
  | V_num of int
  | V_text of string
  | V_player of player_id
  | V_unit
  | V_ctor of {
      name : string;
      fields : (string * t) list;
    }
  | V_tuple of t list
  | V_list of t list
  | V_pile_ref of {
      name : string;
      keys : t list;
    }
  | V_fn of closure
  | V_builtin of string
  | V_pile_ctor of { name : string; arity : int }
  | V_partial of {
      arity : int;
      impl : t list -> t;
    }
  | V_state of state
  | V_view of view
  | V_rng

and closure = {
  params : (string * Types.ty) list;
  body : Tc_ast.texpr;
  captured : (string * t) list;
}

and state = {
  state_config : t;
  state_player_dicts : (player_id * t) list;
  state_piles : pile_entry list;
  state_roster : player_id list;
}

and view = {
  view_config : t;
  view_player_dicts : (player_id * t) list;
  view_piles : view_pile list;
  view_roster : player_id list;
}

and pile_entry = {
  pe_name : string;
  pe_keys : t list;
  pe_cards : t list;
}

and view_pile = {
  vp_name : string;
  vp_keys : t list;
  vp_value : t;
}

type card = t

(* ---------------------------------------------------------------- *)
(* Equality                                                           *)
(* ---------------------------------------------------------------- *)

let rec equal (a : t) (b : t) : bool =
  match a, b with
  | V_num x, V_num y -> x = y
  | V_text x, V_text y -> String.equal x y
  | V_player x, V_player y -> String.equal x y
  | V_unit, V_unit -> true
  | V_ctor a, V_ctor b ->
    String.equal a.name b.name && equal_fields a.fields b.fields
  | V_tuple xs, V_tuple ys -> equal_lists xs ys
  | V_list xs, V_list ys -> equal_lists xs ys

  | V_pile_ref _, _ | _, V_pile_ref _ ->
    invalid_arg "Value.equal: PileRef is not equality-admissible"
  | V_fn _, _ | _, V_fn _ ->
    invalid_arg "Value.equal: function is not equality-admissible"
  | V_builtin _, _ | _, V_builtin _ ->
    invalid_arg "Value.equal: builtin is not equality-admissible"
  | V_pile_ctor _, _ | _, V_pile_ctor _ ->
    invalid_arg "Value.equal: pile constructor is not equality-admissible"
  | V_partial _, _ | _, V_partial _ ->
    invalid_arg "Value.equal: partial application is not equality-admissible"
  | V_state _, _ | _, V_state _ ->
    invalid_arg "Value.equal: State is not equality-admissible"
  | V_view _, _ | _, V_view _ ->
    invalid_arg "Value.equal: View is not equality-admissible"
  | V_rng, _ | _, V_rng ->
    invalid_arg "Value.equal: RNG is not equality-admissible"

  | _, _ -> false

and equal_fields fs gs =
  List.length fs = List.length gs
  && List.for_all2 (fun (n1, v1) (n2, v2) ->
       String.equal n1 n2 && equal v1 v2) fs gs

and equal_lists xs ys =
  List.length xs = List.length ys && List.for_all2 equal xs ys

(* ---------------------------------------------------------------- *)
(* Constructors — must match the field names in [Builtins.seed_types]
   since the interpreter matches on those names at runtime.           *)
(* ---------------------------------------------------------------- *)

let unit = V_unit
let flag_on  = V_ctor { name = "On";  fields = [] }
let flag_off = V_ctor { name = "Off"; fields = [] }
let ok  x = V_ctor { name = "Ok";  fields = [("value", x)] }
let err x = V_ctor { name = "Err"; fields = [("error", x)] }
let ongoing = V_ctor { name = "Ongoing"; fields = [] }
let ended x = V_ctor { name = "Ended"; fields = [("outcome", x)] }
let contents xs =
  V_ctor { name = "Contents"; fields = [("items", V_list xs)] }
let size n = V_ctor { name = "Size"; fields = [("n", V_num n)] }
let masked = V_ctor { name = "Masked"; fields = [] }
let see_all  = V_ctor { name = "SeeAll";  fields = [] }
let see_size = V_ctor { name = "SeeSize"; fields = [] }
let hidden   = V_ctor { name = "Hidden";  fields = [] }
let lt = V_ctor { name = "LT"; fields = [] }
let eq_ord = V_ctor { name = "EQ"; fields = [] }
let gt = V_ctor { name = "GT"; fields = [] }

(* ---------------------------------------------------------------- *)
(* Destructors                                                        *)
(* ---------------------------------------------------------------- *)

let as_flag = function
  | V_ctor { name = "On";  _ } -> Some `On
  | V_ctor { name = "Off"; _ } -> Some `Off
  | _ -> None

let as_result = function
  | V_ctor { name = "Ok";  fields = [("value", v)] } -> Some (`Ok v)
  | V_ctor { name = "Err"; fields = [("error", v)] } -> Some (`Err v)
  | _ -> None

let as_game_status = function
  | V_ctor { name = "Ongoing"; _ } -> Some `Ongoing
  | V_ctor { name = "Ended";   fields = [("outcome", v)] } -> Some (`Ended v)
  | _ -> None

let as_view  = function V_view  v -> Some v | _ -> None
let as_state = function V_state s -> Some s | _ -> None
