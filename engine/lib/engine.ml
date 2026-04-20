type player = string

type option_type =
  | OT_num
  | OT_text
  | OT_enum of string list

type option_value =
  | OV_num of int
  | OV_text of string
  | OV_enum of string

type option_field = {
  name : string;
  ty : option_type;
  default : option_value;
}

type status =
  | Ongoing
  | Ended of string

type log_entry = {
  player : player;
  rendered : string;
}

type setup_error =
  | Invalid_options of string
  | Invalid_seed of string
  | Invalid_players of string
  | Setup_rejected of string
  | Setup_fatal of string

type apply_error =
  | Invalid of string
  | Fatal of string

type parsed = {
  link : Link.t;
  toplevel : (string * Value.t) list;
  builtins : Interp.builtin list;
  options_schema : option_field list;
  source_name : string;
}

type engine_state = {
  state : Value.state;
  rng : Rng.t;
  players : player list;
  options : (string * option_value) list;
  seed : bytes;
  log : log_entry list;
}

(* ---------------------------------------------------------------- *)
(* Options schema and value conversion                                *)
(* ---------------------------------------------------------------- *)

let type_to_option_type (env : Types.env) (ty : Types.ty) : option_type =
  match ty with
  | Types.T_num -> OT_num
  | Types.T_text -> OT_text
  | Types.T_user name ->
    (match Types.lookup_type env name with
     | Some (Types.TI_adt { ctors; _ }) ->
       OT_enum (List.map (fun (c : Types.ctor_info) -> c.ctor_name) ctors)
     | _ -> OT_enum [])
  | _ ->
    (* Typecheck §9 restricts options field types; unreachable in practice. *)
    OT_text

let value_to_option_value (ty : option_type) (v : Value.t) : option_value =
  match ty, v with
  | OT_num,  V_num n -> OV_num n
  | OT_text, V_text s -> OV_text s
  | OT_enum _, V_ctor { name; _ } -> OV_enum name
  | _ ->
    (* Default expressions are type-checked, so this is an engine bug. *)
    OV_text "<bad-default>"

let option_value_to_value : option_value -> Value.t = function
  | OV_num n -> V_num n
  | OV_text s -> V_text s
  | OV_enum ctor -> V_ctor { name = ctor; fields = [] }

let compatible_option_type (ty : option_type) (v : option_value) : bool =
  match ty, v with
  | OT_num, OV_num _ -> true
  | OT_text, OV_text _ -> true
  | OT_enum variants, OV_enum name -> List.mem name variants
  | _ -> false

let validate_options (schema : option_field list)
    (user : (string * option_value) list)
    : ((string * option_value) list, string) result =
  let schema_names = List.map (fun (f : option_field) -> f.name) schema in
  let unknown =
    List.filter (fun (n, _) -> not (List.mem n schema_names)) user
  in
  if unknown <> [] then
    Error (Printf.sprintf "unknown option field(s): %s"
             (String.concat ", " (List.map fst unknown)))
  else
    let rec check acc = function
      | [] -> Ok (List.rev acc)
      | (f : option_field) :: rest ->
        let v =
          match List.assoc_opt f.name user with
          | Some v -> v
          | None -> f.default
        in
        if compatible_option_type f.ty v then
          check ((f.name, v) :: acc) rest
        else
          Error (Printf.sprintf
                   "option '%s' has wrong type for declared schema" f.name)
    in
    check [] schema

let options_to_value (schema : option_field list)
    (validated : (string * option_value) list) : Value.t =
  let fields =
    List.map (fun (f : option_field) ->
      let ov = List.assoc f.name validated in
      (f.name, option_value_to_value ov)) schema
  in
  V_ctor { name = "Options"; fields }

(* ---------------------------------------------------------------- *)
(* parse                                                              *)
(* ---------------------------------------------------------------- *)

let parse ~source_name (src : string) : (parsed, Load_error.t list) result =
  match Lexer.tokenize ~file:source_name src with
  | Error e -> Error [e]
  | Ok toks ->
    match Parser.parse ~source_name toks with
    | Error e -> Error [e]
    | Ok file ->
      match Typecheck.check file with
      | Error es -> Error es
      | Ok (tfile, env) ->
        match Link.link tfile env with
        | Error es -> Error es
        | Ok link ->
          let builtins = Stdlib_impl.all in
          (try
             let toplevel = Interp.build_toplevel link builtins in
             (* Evaluate options defaults at parse time so [options_form]
                can return the concrete defaults without additional plumbing. *)
             let dummy_rng = ref (Rng.of_seed (Bytes.make 16 '\000')) in
             let default_ctx =
               Interp.make_ctx ~link ~builtins ~toplevel
                 ~capability:Cap_toplevel ~roster:[]
                 ~rng:dummy_rng ~temp_scope:None
             in
             let options_schema =
               List.map (fun (f : Link.options_field) ->
                 let ty = type_to_option_type link.env f.ty in
                 let default_v = Interp.eval default_ctx f.default in
                 let default = value_to_option_value ty default_v in
                 { name = f.name; ty; default })
                 link.options_schema
             in
             Ok { link; toplevel; builtins; options_schema; source_name }
           with Value.Fatal msg ->
             Error [Load_error.make Load_error.Link Load_error.no_span
                      (Printf.sprintf
                         "runtime failure during parse: %s" msg)])

let source_name p = p.source_name
let options_form p = p.options_schema

(* The required fns live in [toplevel] as [V_fn] closures. Link gives
   us their [texpr]s but we want the cached closure values for
   [Interp.call]. *)
let lookup_required (p : parsed) (name : string) : Value.t =
  match List.assoc_opt name p.toplevel with
  | Some v -> v
  | None ->
    raise (Value.Fatal
             (Printf.sprintf "required function '%s' missing from toplevel \
                              (linker bug)" name))

(* ---------------------------------------------------------------- *)
(* View computation                                                   *)
(* ---------------------------------------------------------------- *)

let cartesian_product (sets : 'a list list) : 'a list list =
  let rec go = function
    | [] -> [[]]
    | xs :: rest ->
      let tails = go rest in
      List.concat_map (fun x -> List.map (fun t -> x :: t) tails) xs
  in
  go sets

let enumerate_pile_instances (p : Link.pile_info)
    (roster : player list)
    (materialized : Pile.instance list) : Pile.instance list =
  let can_enumerate =
    List.for_all (fun (_, ty) ->
      match ty with Types.T_player_id -> true | _ -> false)
      p.key_params
  in
  let enumerated =
    if can_enumerate then
      let sets =
        List.map (fun _ ->
          List.map (fun pid -> Value.V_player pid) roster)
          p.key_params
      in
      let combos = cartesian_product sets in
      List.map (fun keys -> { Pile.name = p.name; keys }) combos
    else
      []
  in
  let is_same (a : Pile.instance) (b : Pile.instance) =
    String.equal a.name b.name
    && List.length a.keys = List.length b.keys
    && List.for_all2 Value.equal a.keys b.keys
  in
  let mats_for_this =
    List.filter (fun m -> String.equal m.Pile.name p.name) materialized
  in
  enumerated
  @ List.filter (fun m -> not (List.exists (is_same m) enumerated))
      mats_for_this

let compute_view (p : parsed) (es : engine_state)
    (viewer : player) : Value.view =
  let state = es.state in
  let materialized = Pile.materialized_instances state.state_piles in
  let dummy_rng = ref es.rng in
  let vis_ctx =
    Interp.make_ctx ~link:p.link ~builtins:p.builtins ~toplevel:p.toplevel
      ~capability:Cap_visibility ~roster:es.players
      ~rng:dummy_rng ~temp_scope:None
  in
  let view_piles =
    List.concat_map (fun (pd : Link.pile_info) ->
      let insts = enumerate_pile_instances pd es.players materialized in
      List.map (fun (inst : Pile.instance) ->
        (* The visibility expression may reference the pile's key
           params (e.g. [fn (state, viewer) -> if_eq(owner, viewer, ...)])
           so bind the instance's keys to those param names before
           evaluating — otherwise [owner] would fail lookup. *)
        let key_binds =
          try
            List.map2 (fun (name, _ty) v -> (name, v))
              pd.key_params inst.keys
          with Invalid_argument _ -> []
        in
        let inst_ctx = Interp.extend_locals vis_ctx key_binds in
        let vis_fn = Interp.eval inst_ctx pd.visibility in
        let vis_result =
          Interp.call inst_ctx vis_fn
            [V_state state; V_player viewer]
        in
        let cards = Pile.cards_in state.state_piles inst in
        let pv_value =
          match vis_result with
          | V_ctor { name = "SeeAll";  _ } -> Value.contents cards
          | V_ctor { name = "SeeSize"; _ } -> Value.size (List.length cards)
          | V_ctor { name = "Hidden";  _ } -> Value.masked
          | _ -> Value.masked
        in
        { Value.vp_name = inst.name; vp_keys = inst.keys; vp_value = pv_value })
        insts) p.link.pile_decls
  in
  {
    view_config = state.state_config;
    view_player_dicts = state.state_player_dicts;
    view_piles;
    view_roster = es.players;
  }

(* ---------------------------------------------------------------- *)
(* init_state                                                         *)
(* ---------------------------------------------------------------- *)

let init_state (p : parsed) ~options ~players ~seed
    : (engine_state, setup_error) result =
  if Bytes.length seed <> 16 then
    Error (Invalid_seed "seed must be exactly 16 bytes")
  else if players = [] then
    Error (Invalid_players "player list is empty")
  else if
    let sorted = List.sort String.compare players in
    List.length (List.sort_uniq String.compare players)
    <> List.length sorted
  then
    Error (Invalid_players "duplicate player ids")
  else begin
    match validate_options p.options_schema options with
    | Error msg -> Error (Invalid_options msg)
    | Ok validated ->
      let options_v = options_to_value p.options_schema validated in
      let rng = ref (Rng.of_seed seed) in
      let ctx =
        Interp.make_ctx ~link:p.link ~builtins:p.builtins
          ~toplevel:p.toplevel ~capability:Cap_setup
          ~roster:players ~rng ~temp_scope:None
      in
      let players_v = Value.V_list (List.map (fun pl -> Value.V_player pl) players) in
      (try
         let setup_fn = lookup_required p "setup" in
         let result = Interp.call ctx setup_fn [players_v; options_v; V_rng] in
           match Value.as_result result with
           | Some (`Ok state_v) ->
             (match Value.as_state state_v with
              | Some s ->
                Ok {
                  state = s;
                  rng = !rng;
                  players;
                  options = validated;
                  seed;
                  log = [];
                }
              | None ->
                Error (Setup_fatal
                         "setup returned Ok but payload is not a State"))
           | Some (`Err err_v) ->
             let msg = match err_v with
               | V_text s -> s
               | _ -> "setup returned Err with non-Text payload"
             in
             Error (Setup_rejected msg)
           | None ->
             Error (Setup_fatal "setup did not return a Result")
       with
       | Value.Fatal msg -> Error (Setup_fatal msg)
       | Stack_overflow -> Error (Setup_fatal "stack overflow in setup"))
  end

(* ---------------------------------------------------------------- *)
(* validate / apply                                                   *)
(* ---------------------------------------------------------------- *)

(* Shared plumbing: parse the input as an Action, then run validate.
   Returns the parsed Action on success so callers can proceed to
   apply. *)
let parse_and_validate (p : parsed) (es : engine_state)
    ~(viewer : player) ~(input : string)
    : (Value.t * Value.view, apply_error) result =
  let view = compute_view p es viewer in
  let rng = ref es.rng in
  let tta_ctx =
    Interp.make_ctx ~link:p.link ~builtins:p.builtins ~toplevel:p.toplevel
      ~capability:Cap_text_to_action ~roster:es.players
      ~rng ~temp_scope:None
  in
  let val_ctx =
    Interp.make_ctx ~link:p.link ~builtins:p.builtins ~toplevel:p.toplevel
      ~capability:Cap_validate ~roster:es.players
      ~rng ~temp_scope:None
  in
  let unwrap_result v default_fatal =
    match Value.as_result v with
    | Some (`Ok x) -> Ok x
    | Some (`Err (V_text msg)) -> Error (Invalid msg)
    | Some (`Err _) -> Error (Fatal default_fatal)
    | None -> Error (Fatal default_fatal)
  in
  try
    let action_v =
      Interp.call tta_ctx (lookup_required p "text_to_action")
        [V_text input; V_view view; V_player viewer]
    in
    match unwrap_result action_v "text_to_action did not return a Result" with
    | Error e -> Error e
    | Ok action ->
      let val_v =
        Interp.call val_ctx (lookup_required p "validate")
          [V_view view; V_player viewer; action]
      in
      match unwrap_result val_v "validate did not return a Result" with
      | Error e -> Error e
      | Ok _ -> Ok (action, view)
  with
  | Value.Fatal msg -> Error (Fatal msg)
  | Stack_overflow -> Error (Fatal "stack overflow during action parse/validate")

let validate (p : parsed) (es : engine_state)
    ~(player : player) ~(input : string) : (unit, apply_error) result =
  if not (List.mem player es.players) then Error (Invalid "no such player")
  else
    match parse_and_validate p es ~viewer:player ~input with
    | Error e -> Error e
    | Ok _ -> Ok ()

let apply (p : parsed) (es : engine_state)
    ~(player : player) ~(input : string)
    : (engine_state * string, apply_error) result =
  if not (List.mem player es.players) then Error (Invalid "no such player")
  else
    match parse_and_validate p es ~viewer:player ~input with
    | Error e -> Error e
    | Ok (action, _view) ->
      let rng = ref es.rng in
      let scope = Pile.open_scope () in
      let apply_ctx =
        Interp.make_ctx ~link:p.link ~builtins:p.builtins
          ~toplevel:p.toplevel ~capability:Cap_apply
          ~roster:es.players ~rng ~temp_scope:(Some scope)
      in
      let render_ctx =
        Interp.make_ctx ~link:p.link ~builtins:p.builtins
          ~toplevel:p.toplevel ~capability:Cap_action_to_text
          ~roster:es.players ~rng ~temp_scope:None
      in
      try
        let new_state_v =
          Interp.call apply_ctx (lookup_required p "apply")
            [V_state es.state; V_rng; action; V_player player]
        in
        match Value.as_state new_state_v with
        | None -> Error (Fatal "apply did not return a State")
        | Some s ->
          match Pile.close_scope scope s.state_piles with
          | Error msg -> Error (Fatal msg)
          | Ok clean_piles ->
            let clean_state = { s with state_piles = clean_piles } in
            let rendered_v =
              Interp.call render_ctx (lookup_required p "action_to_text")
                [action; V_player player]
            in
            let rendered = match rendered_v with
              | V_text s -> s
              | _ -> "<action_to_text did not return Text>"
            in
            let new_es = {
              es with
              state = clean_state;
              rng = !rng;
              log = es.log @ [{ player; rendered }];
            } in
            Ok (new_es, rendered)
      with
      | Value.Fatal msg -> Error (Fatal msg)
      | Stack_overflow -> Error (Fatal "stack overflow during apply")

(* ---------------------------------------------------------------- *)
(* display / status                                                   *)
(* ---------------------------------------------------------------- *)

let display (p : parsed) (es : engine_state) ~(player : player) : string =
  if not (List.mem player es.players) then " <no such player>"
  else
    let view = compute_view p es player in
    let rng = ref es.rng in
    let ctx =
      Interp.make_ctx ~link:p.link ~builtins:p.builtins
        ~toplevel:p.toplevel ~capability:Cap_view_to_text
        ~roster:es.players ~rng ~temp_scope:None
    in
    try
      match Interp.call ctx (lookup_required p "view_to_text") [V_view view; V_player player] with
      | V_text s -> s
      | _ -> "<view_to_text did not return Text>"
    with
    | Value.Fatal msg -> Printf.sprintf "<fatal: %s>" msg
    | Stack_overflow -> "<stack overflow in view_to_text>"

let status (p : parsed) (es : engine_state) ~(player : player) : status =
  if not (List.mem player es.players) then Ongoing
  else
    let rng = ref es.rng in
    let term_ctx =
      Interp.make_ctx ~link:p.link ~builtins:p.builtins
        ~toplevel:p.toplevel ~capability:Cap_terminal
        ~roster:es.players ~rng ~temp_scope:None
    in
    try
      let status_v =
        Interp.call term_ctx (lookup_required p "terminal") [V_state es.state]
      in
      match Value.as_game_status status_v with
      | Some `Ongoing -> Ongoing
      | Some (`Ended outcome) ->
        let out_ctx =
          Interp.make_ctx ~link:p.link ~builtins:p.builtins
            ~toplevel:p.toplevel ~capability:Cap_outcome_to_text
            ~roster:es.players ~rng ~temp_scope:None
        in
        (match Interp.call out_ctx (lookup_required p "outcome_to_text")
                 [outcome; V_player player] with
         | V_text s -> Ended s
         | _ -> Ended "<outcome_to_text did not return Text>")
      | None -> Ongoing  (* defensive *)
    with
    | Value.Fatal _ | Stack_overflow -> Ongoing

(* ---------------------------------------------------------------- *)
(* Session accessors                                                  *)
(* ---------------------------------------------------------------- *)

let players (es : engine_state) = es.players
let log (es : engine_state) = es.log
let seed (es : engine_state) = es.seed
let options (es : engine_state) = es.options

(* ---------------------------------------------------------------- *)
(* Re-exports                                                         *)
(* ---------------------------------------------------------------- *)

module Error = Load_error

module Dev = struct
  module Load_error = Load_error
  module Token      = Token
  module Lexer      = Lexer
  module Parser     = Parser
  module Ast        = Ast
  module Types      = Types
  module Tc_ast     = Tc_ast
  module Typecheck  = Typecheck
  module Link       = Link
  module Value      = Value
  module Rng        = Rng
  module Pile       = Pile
  module Interp     = Interp

  let raw_state (es : engine_state) : Value.state = es.state
end
