(** Pass 4 of load (runtime.md §11 step 3): verify required declarations
    are present with the correct signatures, then bundle everything the
    runtime needs into a single value.

    Separate from [Typecheck] because the error category is different —
    type errors point at a specific expression, link errors ("missing
    [setup]", "wrong signature on [apply]") are file-level. *)

type pile_info = {
  name : string;
  key_params : (string * Types.ty) list;
  card_ty : Types.ty;                   (** the [C] in [PileRef<C>] *)
  visibility : Typecheck.texpr;         (** already verified total *)
}

type options_field = {
  name : string;
  ty : Types.ty;                        (** restricted per type-system §9 *)
  default : Typecheck.texpr;            (** evaluated once at form-generation time *)
}

type t = private {
  tfile : Typecheck.tfile;
  env : Types.env;

  (* Required type names from type-system §3.2, all resolved. *)
  card_type : string;
  action_type : string;
  outcome_type : string;
  config_type : string;
  player_dict_type : string;

  (* Required function bodies from type-system §3.3. *)
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
    (** Drives [Engine.options_form]. Empty = no [options] block. *)
}

val link : Typecheck.tfile -> Types.env -> (t, Load_error.t list) result
(** Returns every [Link]-category audit failure found, so a ruleset
    missing several required declarations sees them all in one pass.
    The error list is nonempty on failure. *)
