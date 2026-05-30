type pile_info = {
  name : string;
  key_params : (string * Types.ty) list;
  card_ty : Types.ty;
  visibility : Typecheck.texpr;
}

type options_field = {
  name : string;
  ty : Types.ty;
  default : Typecheck.texpr;
}

type t = {
  tfile : Typecheck.tfile;
  env : Types.env;
  card_type : string;
  action_type : string;
  outcome_type : string;
  config_type : string;
  player_dict_type : string;
  setup : Typecheck.texpr;
  validate : Typecheck.texpr;
  apply : Typecheck.texpr;
  terminal : Typecheck.texpr;
  action_to_text : Typecheck.texpr;
  text_to_action : Typecheck.texpr;
  view_to_text : Typecheck.texpr;
  outcome_to_text : Typecheck.texpr;
  pile_decls : pile_info list;
  options_schema : options_field list;
}

(* ---------------------------------------------------------------- *)
(* Audit: required user types (type-system §3.2)                      *)
(* ---------------------------------------------------------------- *)

(* "Missing" errors have no source span — the absence isn't anchored
   anywhere in the file. Shape errors prefer the type's declaration
   span, but [type_decls] doesn't carry one in v0; fall back to
   [no_span] with a clear message. *)
let link_err msg = Load_error.make Load_error.Link Load_error.no_span msg

let audit_types (env : Types.env) : Load_error.t list =
  let check_present name =
    match Types.lookup_type env name with
    | Some (Types.TI_adt _) -> None
    | _ ->
      Some (link_err
              (Printf.sprintf "missing required type '%s' (type-system §3.2)" name))
  in
  let check_record name =
    match Types.lookup_type env name with
    | Some (Types.TI_adt { is_record = true; _ }) -> None
    | Some (Types.TI_adt _) ->
      Some (link_err
              (Printf.sprintf
                 "required type '%s' must be a single-constructor record \
                  (e.g., 'type %s = %s { ... }') (type-system §3.2)"
                 name name name))
    | _ ->
      Some (link_err
              (Printf.sprintf "missing required type '%s' (type-system §3.2)" name))
  in
  List.filter_map (fun x -> x) [
    check_present "Card";
    check_present "Action";
    check_present "Outcome";
    check_record "Config";
    check_record "PlayerDict";
  ]

(* ---------------------------------------------------------------- *)
(* Audit: required functions (type-system §3.3)                       *)
(* ---------------------------------------------------------------- *)

(* Canonical signatures expressed as [Types.ty]. Match must be exact —
   the user wrote each function's signature explicitly, so any
   variation is a real divergence rather than an inference question. *)
let required_fn_signatures : (string * Types.ty) list = [
  "setup",
  Types.T_fn (
    [Types.T_list Types.T_player_id; Types.T_options; Types.T_rng],
    Types.T_result (Types.T_state, Types.T_text));

  "validate",
  Types.T_fn (
    [Types.T_view; Types.T_player_id; Types.T_user "Action"],
    Types.T_result (Types.T_unit, Types.T_text));

  "apply",
  Types.T_fn (
    [Types.T_state; Types.T_rng; Types.T_user "Action"; Types.T_player_id],
    Types.T_state);

  "terminal",
  Types.T_fn (
    [Types.T_state],
    Types.T_game_status (Types.T_user "Outcome"));

  "action_to_text",
  Types.T_fn (
    [Types.T_user "Action"; Types.T_player_id],
    Types.T_text);

  "text_to_action",
  Types.T_fn (
    [Types.T_text; Types.T_view; Types.T_player_id],
    Types.T_result (Types.T_user "Action", Types.T_text));

  "view_to_text",
  Types.T_fn (
    [Types.T_view; Types.T_player_id],
    Types.T_text);

  "outcome_to_text",
  Types.T_fn (
    [Types.T_user "Outcome"; Types.T_player_id],
    Types.T_text);
]

let audit_fns (tfile : Typecheck.tfile) : Load_error.t list =
  let top_fns = Typecheck.top_fns tfile in
  List.filter_map (fun (name, expected) ->
    match List.assoc_opt name top_fns with
    | None ->
      Some (link_err
              (Printf.sprintf
                 "missing required function '%s : %s' (type-system §3.3)"
                 name (Types.string_of_ty expected)))
    | Some (texpr : Typecheck.texpr) ->
      if texpr.ty = expected then None
      else
        Some (Load_error.make Load_error.Link texpr.span
                (Printf.sprintf
                   "required function '%s' has wrong signature: \
                    expected '%s', got '%s' (type-system §3.3)"
                   name
                   (Types.string_of_ty expected)
                   (Types.string_of_ty texpr.ty))))
    required_fn_signatures

(* ---------------------------------------------------------------- *)
(* Bundling                                                           *)
(* ---------------------------------------------------------------- *)

let pile_info_of (name, params, card_ty, vis) : pile_info =
  { name; key_params = params; card_ty; visibility = vis }

let options_field_of (name, ty, default) : options_field =
  { name; ty; default }

let link (tfile : Typecheck.tfile) (env : Types.env)
    : (t, Load_error.t list) result =
  let errs = audit_types env @ audit_fns tfile in
  if errs <> [] then Error errs
  else
    let fn name = List.assoc name (Typecheck.top_fns tfile) in
    Ok {
      tfile;
      env;
      card_type = "Card";
      action_type = "Action";
      outcome_type = "Outcome";
      config_type = "Config";
      player_dict_type = "PlayerDict";
      setup = fn "setup";
      validate = fn "validate";
      apply = fn "apply";
      terminal = fn "terminal";
      action_to_text = fn "action_to_text";
      text_to_action = fn "text_to_action";
      view_to_text = fn "view_to_text";
      outcome_to_text = fn "outcome_to_text";
      pile_decls = List.map pile_info_of (Typecheck.pile_decls tfile);
      options_schema = List.map options_field_of (Typecheck.options_decl tfile);
    }
