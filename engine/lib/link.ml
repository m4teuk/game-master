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

(* The four text-I/O functions are optional: if the ruleset omits them,
   [link] synthesizes a one-liner that delegates to the matching
   [builtin_*_to_*]. User code that declares them explicitly still has
   its signature audited. *)
let text_io_fn_names =
  ["action_to_text"; "text_to_action"; "view_to_text"; "outcome_to_text"]

let audit_fns (tfile : Typecheck.tfile) : Load_error.t list =
  let top_fns = Typecheck.top_fns tfile in
  List.filter_map (fun (name, expected) ->
    match List.assoc_opt name top_fns with
    | None ->
      if List.mem name text_io_fn_names then None   (* will default below *)
      else
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

(* Build a synthesized [TE_lambda] whose body is a single call to
   [builtin_name] applied to the lambda's parameters in order. Used by
   [default_text_io_fn] to stand in for a missing user-declared
   [action_to_text] / [text_to_action] / [view_to_text] / [outcome_to_text]. *)
let synthesize_delegating_fn
    ~(builtin_name : string)
    ~(params : (string * Types.ty) list)
    ~(ret_ty : Types.ty)
  : Typecheck.texpr =
  let sp = Load_error.no_span in
  let mk node ty = { Tc_ast.node; ty; span = sp } in
  let body_args =
    List.map (fun (n, t) -> mk (Tc_ast.TE_var n) t) params
  in
  let fn_ty = Types.T_fn (List.map snd params, ret_ty) in
  let body =
    mk (Tc_ast.TE_app (mk (Tc_ast.TE_var builtin_name) fn_ty, body_args))
       ret_ty
  in
  mk (Tc_ast.TE_lambda { params; body }) fn_ty

let default_text_io_fn (name : string) : Typecheck.texpr =
  match name with
  | "action_to_text" ->
    synthesize_delegating_fn
      ~builtin_name:"builtin_action_to_text"
      ~params:[("a", Types.T_user "Action"); ("p", Types.T_player_id)]
      ~ret_ty:Types.T_text
  | "text_to_action" ->
    synthesize_delegating_fn
      ~builtin_name:"builtin_text_to_action"
      ~params:[("t", Types.T_text); ("v", Types.T_view); ("p", Types.T_player_id)]
      ~ret_ty:(Types.T_result (Types.T_user "Action", Types.T_text))
  | "view_to_text" ->
    synthesize_delegating_fn
      ~builtin_name:"builtin_view_to_text"
      ~params:[("v", Types.T_view); ("p", Types.T_player_id)]
      ~ret_ty:Types.T_text
  | "outcome_to_text" ->
    synthesize_delegating_fn
      ~builtin_name:"builtin_outcome_to_text"
      ~params:[("o", Types.T_user "Outcome"); ("p", Types.T_player_id)]
      ~ret_ty:Types.T_text
  | _ -> raise (Failure "default_text_io_fn: unknown name")

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
    let top = Typecheck.top_fns tfile in
    let fn name = List.assoc name top in
    (* A text-I/O fn that wasn't declared falls back to the builtin. *)
    let fn_or_default_text_io name =
      match List.assoc_opt name top with
      | Some t -> t
      | None -> default_text_io_fn name
    in
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
      action_to_text  = fn_or_default_text_io "action_to_text";
      text_to_action  = fn_or_default_text_io "text_to_action";
      view_to_text    = fn_or_default_text_io "view_to_text";
      outcome_to_text = fn_or_default_text_io "outcome_to_text";
      pile_decls = List.map pile_info_of (Typecheck.pile_decls tfile);
      options_schema = List.map options_field_of (Typecheck.options_decl tfile);
    }
