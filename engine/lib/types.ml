type ty =
  | T_num
  | T_text
  | T_player_id
  | T_unit
  | T_rng
  | T_state
  | T_view
  | T_options
  | T_list of ty
  | T_result of ty * ty
  | T_game_status of ty
  | T_pile_view of ty
  | T_pile_ref of ty
  | T_tuple of ty list
  | T_fn of ty list * ty
  | T_user of string
  | T_var of int

type ctor_info = {
  ctor_name : string;
  owner_type : string;
  fields : (string * ty) list;
  is_record : bool;
}

type type_info =
  | TI_builtin_opaque
  | TI_adt of {
      ctors : ctor_info list;
      is_record : bool;
    }

open Core

type env = {
  types  : type_info Map.M(String).t;
  ctors  : ctor_info Map.M(String).t;
  values : ty        Map.M(String).t;
  param_names : string list Map.M(String).t;
    (* Sparse: only fns and piles whose param names matter for keyword
       argument resolution. Stdlib values are absent from this map. *)
}

let empty = {
  types  = Map.empty (module String);
  ctors  = Map.empty (module String);
  values = Map.empty (module String);
  param_names = Map.empty (module String);
}

let lookup_type        env name = Map.find env.types  name
let lookup_ctor        env name = Map.find env.ctors  name
let lookup_value       env name = Map.find env.values name
let lookup_param_names env name = Map.find env.param_names name

let add_type  env name info = { env with types  = Map.set env.types  ~key:name ~data:info }
let add_ctor  env name info = { env with ctors  = Map.set env.ctors  ~key:name ~data:info }
let add_value env name ty   = { env with values = Map.set env.values ~key:name ~data:ty   }
let add_param_names env name names =
  { env with param_names = Map.set env.param_names ~key:name ~data:names }

(* ---------------------------------------------------------------- *)
(* Equality admissibility (type-system §7.1)                          *)
(* ---------------------------------------------------------------- *)

let rec equality_admissible env ty =
  match ty with
  | T_num | T_text | T_player_id | T_unit -> true
  | T_rng | T_state | T_view -> false
  | T_pile_ref _ -> false
  | T_options -> true (* synthesized record of admissible fields by §9 *)
  | T_list t -> equality_admissible env t
  | T_pile_view t -> equality_admissible env t
  | T_game_status t -> equality_admissible env t
  | T_result (a, b) -> equality_admissible env a && equality_admissible env b
  | T_tuple ts -> List.for_all ts ~f:(equality_admissible env)
  | T_fn _ -> false
  | T_var _ -> true
    (* Optimistic: a variable in a stdlib signature is admissible if its
       eventual instantiation is. The actual check happens after
       unification when [eq] is called. *)
  | T_user name ->
    (match Map.find env.types name with
     | None -> false
     | Some TI_builtin_opaque -> false
     | Some (TI_adt { ctors; _ }) ->
       List.for_all ctors ~f:(fun (c : ctor_info) ->
         List.for_all c.fields ~f:(fun (_, t) -> equality_admissible env t)))

(* ---------------------------------------------------------------- *)
(* Unification (used by stdlib generic instantiation and ctor apps)  *)
(* ---------------------------------------------------------------- *)

let rec apply_subst (s : (int * ty) list) (t : ty) : ty =
  match t with
  | T_var i ->
    (match List.Assoc.find s ~equal:Int.equal i with
     | None -> T_var i
     | Some t' -> apply_subst s t')
  | T_num | T_text | T_player_id | T_unit | T_rng | T_state | T_view
  | T_options | T_user _ -> t
  | T_list t -> T_list (apply_subst s t)
  | T_pile_view t -> T_pile_view (apply_subst s t)
  | T_pile_ref t -> T_pile_ref (apply_subst s t)
  | T_game_status t -> T_game_status (apply_subst s t)
  | T_result (a, b) -> T_result (apply_subst s a, apply_subst s b)
  | T_tuple ts -> T_tuple (List.map ts ~f:(apply_subst s))
  | T_fn (args, ret) ->
    T_fn (List.map args ~f:(apply_subst s), apply_subst s ret)

let rec string_of_ty = function
  | T_num -> "Num"
  | T_text -> "Text"
  | T_player_id -> "PlayerId"
  | T_unit -> "Unit"
  | T_rng -> "RNG"
  | T_state -> "State"
  | T_view -> "View"
  | T_options -> "Options"
  | T_list t -> "List<" ^ string_of_ty t ^ ">"
  | T_result (t, e) -> "Result<" ^ string_of_ty t ^ ", " ^ string_of_ty e ^ ">"
  | T_game_status t -> "GameStatus<" ^ string_of_ty t ^ ">"
  | T_pile_view t -> "PileView<" ^ string_of_ty t ^ ">"
  | T_pile_ref t -> "PileRef<" ^ string_of_ty t ^ ">"
  | T_tuple ts ->
      "(" ^ String.concat ~sep:", " (List.map ~f:string_of_ty ts) ^ ")"
  | T_fn (args, ret) ->
      "(" ^ String.concat ~sep:", " (List.map ~f:string_of_ty args) ^ ") -> "
      ^ string_of_ty ret
  | T_user s -> s
  | T_var n -> Printf.sprintf "'%d" n

let rec unify (s : (int * ty) list) (a : ty) (b : ty)
    : ((int * ty) list, string) Result.t =
  let a = apply_subst s a in
  let b = apply_subst s b in
  match a, b with
  | T_var i, t | t, T_var i ->
    (match t with
     | T_var j when Int.equal i j -> Ok s
     | _ -> Ok ((i, t) :: s))
  | T_num, T_num | T_text, T_text | T_player_id, T_player_id
  | T_unit, T_unit | T_rng, T_rng | T_state, T_state | T_view, T_view
  | T_options, T_options -> Ok s
  | T_list a1, T_list b1
  | T_pile_view a1, T_pile_view b1
  | T_pile_ref a1, T_pile_ref b1
  | T_game_status a1, T_game_status b1 -> unify s a1 b1
  | T_result (a1, a2), T_result (b1, b2) ->
    Result.bind (unify s a1 b1) ~f:(fun s' -> unify s' a2 b2)
  | T_tuple xs, T_tuple ys
    when Int.equal (List.length xs) (List.length ys) ->
    List.fold2_exn xs ys ~init:(Ok s) ~f:(fun acc x y ->
      Result.bind acc ~f:(fun s' -> unify s' x y))
  | T_fn (a_args, a_ret), T_fn (b_args, b_ret)
    when Int.equal (List.length a_args) (List.length b_args) ->
    let after_args =
      List.fold2_exn a_args b_args ~init:(Ok s) ~f:(fun acc x y ->
        Result.bind acc ~f:(fun s' -> unify s' x y))
    in
    Result.bind after_args ~f:(fun s' -> unify s' a_ret b_ret)
  | T_user x, T_user y when String.equal x y -> Ok s
  | T_user "Options", T_options | T_options, T_user "Options" -> Ok s
    (* The synthesized [Options] type can flow as [T_user "Options"]
       (via lookup_ctor's owner_type) or as [T_options] (via the
       resolver mapping the type-expression keyword "Options"). Treat
       as the same nominal type. *)
  | T_user "Unit", T_unit | T_unit, T_user "Unit" -> Ok s
    (* Same trick as Options: [Unit] is seeded as a single-ctor ADT
       owned by "Unit", but the type expression "Unit" resolves to
       [T_unit]. Treat the two spellings as the same nominal type. *)
  | _ ->
    Error (Printf.sprintf "cannot unify '%s' and '%s'"
             (string_of_ty a) (string_of_ty b))

let unify_instantiation ~params ~args ~return =
  if List.length params <> List.length args then
    Error (Printf.sprintf "arity mismatch: %d parameter(s), %d argument(s)"
             (List.length params) (List.length args))
  else
    let final =
      List.fold2_exn params args ~init:(Ok []) ~f:(fun acc p a ->
        Result.bind acc ~f:(fun s -> unify s p a))
    in
    Result.map final ~f:(fun s -> apply_subst s return)
