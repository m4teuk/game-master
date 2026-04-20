(* Tc_expr: bidirectional type-checker for expressions and patterns.

   Sections:
     C1  literals, var, paren, neg, bin
     C2  tuple, list
     C3  ctor, record (construction + update)
     C4  check_pattern (all forms)
     C5  application + Types.unify_instantiation
     C6  lambda, let
     C7  match (without exhaustiveness — see Tc_exhaustiveness for C8)
*)

(* ---------------------------------------------------------------- *)
(* Helpers                                                            *)
(* ---------------------------------------------------------------- *)

let rec has_tvar = function
  | Types.T_var _ -> true
  | Types.T_num | Types.T_text | Types.T_player_id
  | Types.T_unit | Types.T_rng | Types.T_state
  | Types.T_view | Types.T_options | Types.T_user _ -> false
  | Types.T_list t | Types.T_pile_view t
  | Types.T_pile_ref t | Types.T_game_status t -> has_tvar t
  | Types.T_result (a, b) -> has_tvar a || has_tvar b
  | Types.T_tuple ts -> List.exists has_tvar ts
  | Types.T_fn (args, ret) -> List.exists has_tvar args || has_tvar ret

(* Bidirectional check: report a mismatch when [expected] is given and
   does not unify with [actual]. Returns the type to use downstream
   (the substituted actual on success, [expected] as a stand-in on
   error so traversal can continue). *)
let assert_compatible errs span ~expected ~actual =
  match expected with
  | None -> actual
  | Some t ->
    (match Types.unify [] t actual with
     | Ok subst -> Types.apply_subst subst actual
     | Error _ ->
       Tc_errors.report errs span
         (Printf.sprintf "type mismatch: expected '%s', got '%s'"
            (Types.string_of_ty t) (Types.string_of_ty actual));
       t)

(* The (possibly polymorphic) result type produced by a ctor call.
   For generic builtins, returns a [T_var]-tagged template that
   unifies down to the concrete instance against either the args or
   an expected return. *)
let ctor_result_template (info : Types.ctor_info) : Types.ty =
  match info.owner_type with
  | "Result"     -> Types.T_result (Types.T_var 0, Types.T_var 1)
  | "GameStatus" -> Types.T_game_status (Types.T_var 0)
  | "PileView"   -> Types.T_pile_view (Types.T_var 0)
  | "Options"    -> Types.T_options
  | "Unit"       -> Types.T_unit
  | name         -> Types.T_user name

(* Field types of a ctor, instantiated to fit a concrete owner instance.
   E.g., for [Ok] (whose declared field is [value : T_var 0]) against
   [T_result(T_num, T_text)], yields [[("value", T_num)]]. *)
let ctor_field_types_for (info : Types.ctor_info) (instance : Types.ty)
    : ((string * Types.ty) list, string) result =
  let template = ctor_result_template info in
  match Types.unify [] template instance with
  | Ok s ->
    Ok (List.map
          (fun (n, t) -> (n, Types.apply_subst s t)) info.fields)
  | Error msg -> Error msg

(* Reorder user-supplied call args to declared-name order. Reports
   arity, missing-name, unknown-keyword, and duplicate-keyword errors.
   Returns a list of [(declared_name, expr)] in declared order; on
   missing args, fills with a dummy [E_num 0] placeholder so the
   caller can continue. *)
let reorder_args errs span ~declared (user_args : Ast.arg list)
    : (string * Ast.expr) list =
  let positionals = List.filter_map (function
    | Ast.A_pos e -> Some e | Ast.A_kw _ -> None) user_args in
  let keywords = List.filter_map (function
    | Ast.A_kw (n, e) -> Some (n, e) | Ast.A_pos _ -> None) user_args in
  let n_pos = List.length positionals in
  let n_dec = List.length declared in
  let n_args = List.length user_args in
  if n_args <> n_dec then
    Tc_errors.report errs span
      (Printf.sprintf "expected %d argument(s), got %d" n_dec n_args);
  if n_pos > n_dec then
    Tc_errors.report errs span
      "more positional arguments than parameters";
  List.iter (fun (n, _) ->
    if not (List.mem n declared) then
      Tc_errors.report errs span
        (Printf.sprintf "unknown argument name '%s'" n)) keywords;
  let seen_kw = ref [] in
  List.iter (fun (n, _) ->
    if List.mem n !seen_kw then
      Tc_errors.report errs span
        (Printf.sprintf "duplicate argument name '%s'" n)
    else seen_kw := n :: !seen_kw) keywords;
  let dummy = Ast.E_num (0, span) in
  List.mapi (fun i name ->
    let expr =
      if i < n_pos then List.nth positionals i
      else
        match List.assoc_opt name keywords with
        | Some e -> e
        | None ->
          Tc_errors.report errs span
            (Printf.sprintf "missing argument '%s'" name);
          dummy
    in
    (name, expr)) declared

(* ---------------------------------------------------------------- *)
(* Mutual recursion: check_expr / check_pattern / check_application  *)
(* ---------------------------------------------------------------- *)

let rec check_expr (env : Types.env) (errs : Tc_errors.t)
    ~(expected : Types.ty option) (e : Ast.expr) : Tc_ast.texpr =
  match e with
  (* ----- C1 ----- *)
  | Ast.E_num (n, span) ->
    let ty = assert_compatible errs span ~expected ~actual:Types.T_num in
    { node = TE_num n; ty; span }
  | Ast.E_text (s, span) ->
    let ty = assert_compatible errs span ~expected ~actual:Types.T_text in
    { node = TE_text s; ty; span }
  | Ast.E_var (name, span) ->
    let actual = match Types.lookup_value env name with
      | Some t -> t
      | None ->
        Tc_errors.report errs span
          (Printf.sprintf "unknown value '%s'" name);
        Types.T_unit
    in
    let ty = assert_compatible errs span ~expected ~actual in
    { node = TE_var name; ty; span }
  | Ast.E_paren (inner, _span) ->
    check_expr env errs ~expected inner
  | Ast.E_neg (inner, span) ->
    let inner_t = check_expr env errs ~expected:(Some Types.T_num) inner in
    let ty = assert_compatible errs span ~expected ~actual:Types.T_num in
    { node = TE_neg inner_t; ty; span }
  | Ast.E_bin (op, l, r, span) ->
    let l_t = check_expr env errs ~expected:(Some Types.T_num) l in
    let r_t = check_expr env errs ~expected:(Some Types.T_num) r in
    let ty = assert_compatible errs span ~expected ~actual:Types.T_num in
    { node = TE_bin (op, l_t, r_t); ty; span }

  (* ----- C2 ----- *)
  | Ast.E_tuple (es, span) ->
    let n = List.length es in
    let sub_expected = match expected with
      | Some (Types.T_tuple ts) when List.length ts = n ->
        List.map (fun t -> Some t) ts
      | _ -> List.init n (fun _ -> None)
    in
    let elems = List.map2 (fun exp ei ->
      check_expr env errs ~expected:exp ei) sub_expected es in
    let actual = Types.T_tuple
      (List.map (fun (e : Tc_ast.texpr) -> e.ty) elems) in
    let ty = assert_compatible errs span ~expected ~actual in
    { node = TE_tuple elems; ty; span }
  | Ast.E_list (es, span) ->
    let elem_ty, elems =
      match expected, es with
      | Some (Types.T_list t), _ ->
        let elems = List.map (fun e ->
          check_expr env errs ~expected:(Some t) e) es in
        t, elems
      | _, [] ->
        Tc_errors.report errs span
          "cannot infer element type of empty list (provide a context type)";
        Types.T_unit, []
      | _, first :: rest ->
        let first_t = check_expr env errs ~expected:None first in
        let rest_t = List.map (fun e ->
          check_expr env errs ~expected:(Some first_t.ty) e) rest in
        first_t.ty, first_t :: rest_t
    in
    let actual = Types.T_list elem_ty in
    let ty = assert_compatible errs span ~expected ~actual in
    { node = TE_list elems; ty; span }

  (* ----- C3 ----- *)
  | Ast.E_ctor (name, span) ->
    (* Pile names parse as TYPE_IDENT and arrive here, but they live
       in the value namespace (per type-system §8.1). If [name] isn't a
       constructor, fall back to a value lookup before failing. *)
    (match Types.lookup_ctor env name with
     | Some _ ->
       check_ctor_app env errs span ~expected ~ctor_name:name ~user_args:[]
         ~as_record:false
     | None ->
       (match Types.lookup_value env name with
        | Some t ->
          let ty = assert_compatible errs span ~expected ~actual:t in
          { node = TE_var name; ty; span }
        | None ->
          Tc_errors.report errs span
            (Printf.sprintf "unknown name '%s'" name);
          let ty = match expected with Some t -> t | None -> Types.T_unit in
          { node = TE_ctor (name, []); ty; span }))
  | Ast.E_record { ctor; body; span } ->
    check_record env errs span ~expected ~ctor ~body

  (* ----- C5: function or positional-ctor application -----
     A bare TYPE_IDENT can be either a constructor or a value (pile
     name), and the same name can collide across the two namespaces
     (e.g. crazy_eights' [JustDrawn] is both a Phase ctor and a pile).
     Disambiguate by arity: a ctor whose declared arity matches the
     call wins; otherwise fall through to the value lookup. If
     neither resolves, run the ctor path so the error reads
     "unknown constructor" or "wrong arity" coherently. *)
  | Ast.E_app (Ast.E_ctor (name, ctor_span), user_args, span) ->
    let n_args = List.length user_args in
    let ctor_matches =
      match Types.lookup_ctor env name with
      | Some info -> List.length info.fields = n_args
      | None -> false
    in
    if ctor_matches then
      check_ctor_app env errs span ~expected ~ctor_name:name ~user_args
        ~as_record:false
    else if Option.is_some (Types.lookup_value env name) then
      check_fn_app env errs span ~expected
        ~fn_expr:(Ast.E_var (name, ctor_span)) ~user_args
    else
      check_ctor_app env errs span ~expected ~ctor_name:name ~user_args
        ~as_record:false
  | Ast.E_app (fn_expr, user_args, span) ->
    check_fn_app env errs span ~expected ~fn_expr ~user_args

  (* ----- C6 ----- *)
  | Ast.E_lambda { params; body; span } ->
    check_lambda env errs span ~expected ~params ~body
  | Ast.E_let { pat; value; body; span } ->
    check_let env errs span ~expected ~pat ~value ~body

  (* ----- C7 ----- *)
  | Ast.E_match { scrutinee; arms; span } ->
    check_match env errs span ~expected ~scrutinee ~arms

(* ----- C3: ctor application (named-field path used by E_ctor and
   E_record-construction; positional path used by E_app on E_ctor). *)
and check_ctor_app env errs span ~expected ~ctor_name
    ~(user_args : Ast.arg list) ~as_record =
  let _ = as_record in
  match Types.lookup_ctor env ctor_name with
  | None ->
    Tc_errors.report errs span
      (Printf.sprintf "unknown constructor '%s'" ctor_name);
    let ty = match expected with Some t -> t | None -> Types.T_unit in
    { node = TE_ctor (ctor_name, []); ty; span }
  | Some info ->
    let declared = info.fields in
    let ordered = reorder_args errs span
      ~declared:(List.map fst declared) user_args
    in
    let result_ty, typed_fields =
      check_application_seq env errs span ~expected
        ~param_decls:declared
        ~ret_template:(ctor_result_template info)
        ~ordered_args:ordered
    in
    { node = TE_ctor (ctor_name, typed_fields); ty = result_ty; span }

(* ----- C3: record construction or update via { ... } syntax. *)
and check_record env errs span ~expected ~ctor ~(body : Ast.record_body) =
  match Types.lookup_ctor env ctor with
  | None ->
    Tc_errors.report errs span
      (Printf.sprintf "unknown constructor '%s'" ctor);
    let ty = match expected with Some t -> t | None -> Types.T_unit in
    { node = TE_ctor (ctor, []); ty; span }
  | Some info ->
    let declared = info.fields in
    let declared_names = List.map fst declared in
    (* Validate user field names: unknown / duplicate. *)
    let seen = ref [] in
    List.iter (fun (n, _) ->
      if List.mem n !seen then
        Tc_errors.report errs span
          (Printf.sprintf "duplicate field '%s' in '%s'" n ctor)
      else if not (List.mem n declared_names) then
        Tc_errors.report errs span
          (Printf.sprintf "constructor '%s' has no field '%s'" ctor n)
      else seen := n :: !seen) body.fields;
    (match body.spread with
     | None ->
       (* Construction: every declared field must be provided. *)
       List.iter (fun (n, _) ->
         if not (List.mem_assoc n body.fields) then
           Tc_errors.report errs span
             (Printf.sprintf "missing field '%s' in '%s'" n ctor)) declared;
       (* Reorder to declaration order, filling missing with dummy. *)
       let dummy = Ast.E_num (0, span) in
       let ordered =
         List.map (fun (n, _) ->
           let e = match List.assoc_opt n body.fields with
             | Some e -> e
             | None -> dummy
           in (n, e)) declared
       in
       let result_ty, typed_fields =
         check_application_seq env errs span ~expected
           ~param_decls:declared
           ~ret_template:(ctor_result_template info)
           ~ordered_args:ordered
       in
       { node = TE_ctor (ctor, typed_fields); ty = result_ty; span }
     | Some spread_expr ->
       (* Update: ctor must be a single-ctor record. *)
       if not info.is_record then
         Tc_errors.report errs span
           (Printf.sprintf
              "constructor '%s' does not support record-update syntax \
               (only single-constructor record types do)" ctor);
       let result_template = ctor_result_template info in
       let exp_for_spread = match expected with
         | Some t -> Some t
         | None -> if has_tvar result_template then None else Some result_template
       in
       let spread_t = check_expr env errs ~expected:exp_for_spread spread_expr in
       (* For overrides: type-check each provided field against its
          declared type; missing fields are inherited from the spread. *)
       let typed_overrides =
         List.filter_map (fun (n, decl_ty) ->
           match List.assoc_opt n body.fields with
           | None -> None
           | Some user_e ->
             let exp = if has_tvar decl_ty then None else Some decl_ty in
             let texpr = check_expr env errs ~expected:exp user_e in
             Some (n, texpr))
           declared
       in
       let result_ty = match expected with
         | Some t -> t
         | None -> spread_t.ty
       in
       { node = TE_record_update {
           ctor;
           spread = spread_t;
           fields = typed_overrides;
         };
         ty = result_ty;
         span = span })

(* ----- C5: shared application driver. Sequentially type-checks each
   arg against the (substituted) declared type, threading a unification
   substitution to handle stdlib generics and ctor T_vars. *)
and check_application_seq env errs span ~expected
    ~(param_decls : (string * Types.ty) list)
    ~(ret_template : Types.ty)
    ~(ordered_args : (string * Ast.expr) list)
    : Types.ty * (string * Tc_ast.texpr) list =
  if List.length param_decls <> List.length ordered_args then
    let ty = match expected with Some t -> t | None -> ret_template in
    (ty, [])
  else
    let init_subst = match expected with
      | None -> []
      | Some e ->
        (match Types.unify [] ret_template e with
         | Ok s -> s
         | Error _ -> [])
    in
    let final_subst, rev_typed =
      List.fold_left2 (fun (s, acc) (decl_name, decl_ty) (_, arg_expr) ->
        let exp_ty = Types.apply_subst s decl_ty in
        (* Always propagate the (possibly partial) expected type — even
           T_var-bearing — so lambdas can pull their param types from
           the [T_fn] shape and assert_compatible's local unification
           can resolve T_vars against the actual. *)
        let texpr = check_expr env errs ~expected:(Some exp_ty) arg_expr in
        let s' = match Types.unify s decl_ty texpr.ty with
          | Ok s' -> s'
          | Error msg ->
            Tc_errors.report errs texpr.span msg;
            s
        in
        (s', (decl_name, texpr) :: acc))
        (init_subst, []) param_decls ordered_args
    in
    let result_ty = Types.apply_subst final_subst ret_template in
    let result_ty = match expected with
      | None -> result_ty
      | Some e ->
        (match Types.unify final_subst result_ty e with
         | Ok s' -> Types.apply_subst s' result_ty
         | Error msg ->
           Tc_errors.report errs span msg;
           e)
    in
    (result_ty, List.rev rev_typed)

(* ----- C5: function call. Special-cased on [eq] / [if_eq] to enforce
   equality-admissibility per type-system §7.1. *)
and check_fn_app env errs span ~expected ~fn_expr
    ~(user_args : Ast.arg list) =
  let fn_t = check_expr env errs ~expected:None fn_expr in
  match fn_t.ty with
  | Types.T_fn (param_tys, ret_ty) ->
    let n = List.length param_tys in
    let param_names = match fn_expr with
      | Ast.E_var (fname, _) ->
        (match Types.lookup_param_names env fname with
         | Some names when List.length names = n -> names
         | _ -> List.init n (fun i -> Printf.sprintf "_arg%d" i))
      | _ -> List.init n (fun i -> Printf.sprintf "_arg%d" i)
    in
    let ordered = reorder_args errs span ~declared:param_names user_args in
    let param_decls = List.combine param_names param_tys in
    let result_ty, typed_pairs =
      check_application_seq env errs span ~expected
        ~param_decls ~ret_template:ret_ty
        ~ordered_args:ordered
    in
    let typed_args = List.map snd typed_pairs in
    (* Equality-admissibility checks per type-system §7.1 / stdlib §9, §11.
       For [eq]/[if_eq] the operands' shared type is the first arg's ty.
       For [member]/[next_in_cycle] the constraint is on the list's
       element type. *)
    let report_admissibility_error t =
      Tc_errors.report errs span
        (Printf.sprintf
           "type '%s' is not equality-admissible (type-system §7.1)"
           (Types.string_of_ty t))
    in
    (match fn_expr with
     | Ast.E_var (("eq" | "if_eq"), _) when typed_args <> [] ->
       let arg_ty = (List.hd typed_args).Tc_ast.ty in
       if not (Types.equality_admissible env arg_ty) then
         report_admissibility_error arg_ty
     | Ast.E_var (("member" | "next_in_cycle"), _) when typed_args <> [] ->
       (match (List.hd typed_args).Tc_ast.ty with
        | Types.T_list elem_ty ->
          if not (Types.equality_admissible env elem_ty) then
            report_admissibility_error elem_ty
        | _ -> ())
     | _ -> ());
    { node = TE_app (fn_t, typed_args); ty = result_ty; span }
  | _ ->
    Tc_errors.report errs span
      (Printf.sprintf "expression of type '%s' is not callable"
         (Types.string_of_ty fn_t.ty));
    let ty = match expected with Some t -> t | None -> Types.T_unit in
    { node = TE_app (fn_t, []); ty; span }

(* ----- C6: lambda. Bidirectional — uses an expected [T_fn] to
   resolve unannotated parameters; otherwise requires annotations. *)
and check_lambda env errs span ~expected ~(params : Ast.param list) ~body =
  let n = List.length params in
  let exp_param_tys, exp_ret_ty = match expected with
    | Some (Types.T_fn (pts, rt)) when List.length pts = n ->
      List.map (fun t -> Some t) pts, Some rt
    | _ -> List.init n (fun _ -> None), None
  in
  let actual_param_tys =
    List.map2 (fun (p : Ast.param) exp_pt ->
      match p.annot with
      | Some te -> Tc_resolver.resolve_ty env errs te
      | None ->
        (match exp_pt with
         | Some t -> t
         | None ->
           Tc_errors.report errs p.span
             (Printf.sprintf
                "lambda parameter '%s' needs a type annotation \
                 (no expected type to infer from)" p.name);
           Types.T_unit))
      params exp_param_tys
  in
  let body_env =
    List.fold_left2 (fun e (p : Ast.param) ty ->
      Types.add_value e p.name ty)
      env params actual_param_tys
  in
  let body_t = check_expr body_env errs ~expected:exp_ret_ty body in
  let lambda_ty = Types.T_fn (actual_param_tys, body_t.ty) in
  let final_ty = match expected with
    | None -> lambda_ty
    | Some t ->
      (match Types.unify [] t lambda_ty with
       | Ok s -> Types.apply_subst s lambda_ty
       | Error _ ->
         Tc_errors.report errs span
           (Printf.sprintf "type mismatch: expected '%s', got '%s'"
              (Types.string_of_ty t) (Types.string_of_ty lambda_ty));
         t)
  in
  let typed_params = List.map2
    (fun (p : Ast.param) ty -> (p.name, ty))
    params actual_param_tys
  in
  { node = TE_lambda { params = typed_params; body = body_t };
    ty = final_ty; span }

(* ----- C6: let. Pattern must be irrefutable (already checked at
   top-level by Tc_pass_lets; for nested lets, check inline). *)
and check_let env errs span ~expected ~pat ~value ~body =
  let value_t = check_expr env errs ~expected:None value in
  let tpat, bindings =
    check_pattern env errs ~expected:value_t.ty pat
  in
  if not (is_irrefutable_inline env pat) then
    Tc_errors.report errs (Ast.span_of_pattern pat)
      "let-binding pattern must be irrefutable (type-system §6.5)";
  let body_env =
    List.fold_left (fun e (n, t) -> Types.add_value e n t) env bindings
  in
  let body_t = check_expr body_env errs ~expected body in
  { node = TE_let { pat = tpat; value = value_t; body = body_t };
    ty = body_t.ty; span }

(* ----- C7: match. Each arm's pattern is checked against the scrutinee
   type; arm bodies must agree on a single result type. Exhaustiveness
   is delegated to [Tc_exhaustiveness].

   The first arm is checked against the original [expected] (which may
   contain T_var leftovers from a stdlib generic call). [assert_compatible]
   inside that arm's body locally unifies and returns the substituted
   actual type, which we then use as the match's running result type
   for subsequent arms. This keeps the match's result *concrete* even
   when [expected] was a T_var template — preventing the T_var from
   leaking into the surrounding let binding and poisoning unrelated
   stdlib calls downstream. *)
and check_match env errs span ~expected ~scrutinee ~arms =
  let scrutinee_t = check_expr env errs ~expected:None scrutinee in
  let scrut_ty = scrutinee_t.ty in
  (match scrut_ty with
   | Types.T_state | Types.T_view | Types.T_rng | Types.T_pile_ref _ ->
     Tc_errors.report errs scrutinee_t.span
       (Printf.sprintf
          "cannot pattern-match on opaque type '%s' (type-system §1.1)"
          (Types.string_of_ty scrut_ty))
   | Types.T_fn _ ->
     Tc_errors.report errs scrutinee_t.span
       "cannot pattern-match on a function value"
   | _ -> ());
  let check_arm ~arm_expected (pat, body) =
    let tpat, bindings = check_pattern env errs ~expected:scrut_ty pat in
    let arm_env =
      List.fold_left (fun e (n, t) -> Types.add_value e n t) env bindings
    in
    let body_t = check_expr arm_env errs ~expected:arm_expected body in
    (tpat, body_t)
  in
  let typed_arms, final_ty =
    match arms with
    | [] -> ([], (match expected with Some t -> t | None -> Types.T_unit))
    | first :: rest ->
      let (tpat0, body_t0) as arm0 = check_arm ~arm_expected:expected first in
      let common_ty = body_t0.ty in
      let typed_rest =
        List.map (check_arm ~arm_expected:(Some common_ty)) rest
      in
      let _ = tpat0 in
      (arm0 :: typed_rest, common_ty)
  in
  Tc_exhaustiveness.check env errs
    ~scrutinee_ty:scrut_ty ~scrutinee_span:scrutinee_t.span
    (List.map fst arms);
  { node = TE_match { scrutinee = scrutinee_t; arms = typed_arms };
    ty = final_ty; span }

(* ----- C4: pattern checking. Returns the typed pattern plus the
   list of variable bindings it introduces. *)
and check_pattern (env : Types.env) (errs : Tc_errors.t)
    ~(expected : Types.ty) (p : Ast.pattern)
    : Tc_ast.tpattern * (string * Types.ty) list =
  match p with
  | Ast.P_wild span ->
    ({ pat_node = TP_wild; pat_ty = expected; pat_span = span }, [])
  | Ast.P_var (name, span) ->
    ({ pat_node = TP_var name; pat_ty = expected; pat_span = span },
     [(name, expected)])
  | Ast.P_num (n, span) ->
    (match Types.unify [] expected Types.T_num with
     | Ok _ -> ()
     | Error _ ->
       Tc_errors.report errs span
         (Printf.sprintf
            "numeric pattern matches Num, but scrutinee has type '%s'"
            (Types.string_of_ty expected)));
    ({ pat_node = TP_num n; pat_ty = expected; pat_span = span }, [])
  | Ast.P_ctor { name; fields; has_rest; span } ->
    check_pattern_ctor env errs span ~expected ~name
      ~fields:(match fields with Some fs -> fs | None -> [])
      ~has_rest
  | Ast.P_ctor_pos { name; args; span } ->
    check_pattern_ctor_pos env errs span ~expected ~name ~args
  | Ast.P_tuple (ps, span) ->
    let n = List.length ps in
    let elem_tys = match expected with
      | Types.T_tuple ts when List.length ts = n -> ts
      | _ ->
        Tc_errors.report errs span
          (Printf.sprintf
             "tuple pattern of arity %d does not match scrutinee type '%s'"
             n (Types.string_of_ty expected));
        List.init n (fun _ -> Types.T_unit)
    in
    let typed, all_bindings =
      List.fold_left2 (fun (acc, binds) sub_ty sub_p ->
        let tp, b = check_pattern env errs ~expected:sub_ty sub_p in
        (tp :: acc, b @ binds))
        ([], []) elem_tys ps
    in
    ({ pat_node = TP_tuple (List.rev typed);
       pat_ty = expected; pat_span = span }, all_bindings)
  | Ast.P_list_exact (ps, span) ->
    let elem_ty = match expected with
      | Types.T_list t -> t
      | _ ->
        Tc_errors.report errs span
          (Printf.sprintf
             "list pattern does not match scrutinee type '%s'"
             (Types.string_of_ty expected));
        Types.T_unit
    in
    let typed, all_bindings =
      List.fold_left (fun (acc, binds) sub_p ->
        let tp, b = check_pattern env errs ~expected:elem_ty sub_p in
        (tp :: acc, b @ binds))
        ([], []) ps
    in
    ({ pat_node = TP_list_exact (List.rev typed);
       pat_ty = expected; pat_span = span }, all_bindings)
  | Ast.P_list_cons { heads; rest; span } ->
    let elem_ty = match expected with
      | Types.T_list t -> t
      | _ ->
        Tc_errors.report errs span
          (Printf.sprintf
             "list pattern does not match scrutinee type '%s'"
             (Types.string_of_ty expected));
        Types.T_unit
    in
    let typed_heads, head_bindings =
      List.fold_left (fun (acc, binds) sub_p ->
        let tp, b = check_pattern env errs ~expected:elem_ty sub_p in
        (tp :: acc, b @ binds))
        ([], []) heads
    in
    let rest_bindings = match rest with
      | Some n -> [(n, Types.T_list elem_ty)]
      | None -> []
    in
    ({ pat_node = TP_list_cons {
         heads = List.rev typed_heads;
         rest;
       };
       pat_ty = expected; pat_span = span },
     rest_bindings @ head_bindings)

(* ----- C4: named-field constructor pattern. *)
and check_pattern_ctor env errs span ~expected ~name
    ~(fields : Ast.field_pat list) ~has_rest =
  match Types.lookup_ctor env name with
  | None ->
    Tc_errors.report errs span
      (Printf.sprintf "unknown constructor '%s'" name);
    ({ pat_node = TP_wild; pat_ty = expected; pat_span = span }, [])
  | Some info ->
    (match ctor_field_types_for info expected with
     | Error _ ->
       Tc_errors.report errs span
         (Printf.sprintf
            "constructor '%s' belongs to type '%s', does not match \
             scrutinee type '%s'"
            name info.owner_type (Types.string_of_ty expected));
       ({ pat_node = TP_wild; pat_ty = expected; pat_span = span }, [])
     | Ok declared ->
       List.iter (fun (fp : Ast.field_pat) ->
         if not (List.mem_assoc fp.field declared) then
           Tc_errors.report errs fp.span
             (Printf.sprintf "constructor '%s' has no field '%s'"
                name fp.field)) fields;
       let typed_fields, all_bindings =
         List.fold_left (fun (acc, binds) (fname, fty) ->
           match List.find_opt
                   (fun (fp : Ast.field_pat) -> String.equal fp.field fname)
                   fields with
           | Some fp ->
             let sub_pat = match fp.sub with
               | Some p -> p
               | None -> Ast.P_var (fname, fp.span)
             in
             let tp, b = check_pattern env errs ~expected:fty sub_pat in
             ((fname, tp) :: acc, b @ binds)
           | None ->
             if not has_rest then
               Tc_errors.report errs span
                 (Printf.sprintf
                    "missing field '%s' in pattern for '%s' (use '..' to ignore)"
                    fname name);
             (acc, binds))
           ([], []) declared
       in
       ({ pat_node = TP_ctor {
            name;
            fields = List.rev typed_fields;
            has_rest;
          };
          pat_ty = expected; pat_span = span },
        all_bindings))

(* ----- C4: positional constructor pattern (e.g. [Ok(x)]). Equivalent
   to the named form with implicit field-order assignment. *)
and check_pattern_ctor_pos env errs span ~expected ~name ~args =
  match Types.lookup_ctor env name with
  | None ->
    Tc_errors.report errs span
      (Printf.sprintf "unknown constructor '%s'" name);
    ({ pat_node = TP_wild; pat_ty = expected; pat_span = span }, [])
  | Some info ->
    (match ctor_field_types_for info expected with
     | Error _ ->
       Tc_errors.report errs span
         (Printf.sprintf
            "constructor '%s' belongs to type '%s', does not match \
             scrutinee type '%s'"
            name info.owner_type (Types.string_of_ty expected));
       ({ pat_node = TP_wild; pat_ty = expected; pat_span = span }, [])
     | Ok declared ->
       let n_decl = List.length declared in
       let n_args = List.length args in
       if n_decl <> n_args then begin
         Tc_errors.report errs span
           (Printf.sprintf
              "constructor '%s' takes %d field(s), pattern provides %d"
              name n_decl n_args);
         ({ pat_node = TP_wild; pat_ty = expected; pat_span = span }, [])
       end else
         let typed_fields, all_bindings =
           List.fold_left2 (fun (acc, binds) (fname, fty) sub_p ->
             let tp, b = check_pattern env errs ~expected:fty sub_p in
             ((fname, tp) :: acc, b @ binds))
             ([], []) declared args
         in
         ({ pat_node = TP_ctor {
              name;
              fields = List.rev typed_fields;
              has_rest = false;
            };
            pat_ty = expected; pat_span = span },
          all_bindings))

(* Inline irrefutability check for nested [let] expressions. Mirrors
   [Tc_pass_lets.pat_irrefutable] — kept duplicated rather than
   exposed because they live in different layers and the rule is
   short. *)
and is_irrefutable_inline env (p : Ast.pattern) : bool =
  match p with
  | Ast.P_wild _ | Ast.P_var _ -> true
  | Ast.P_num _ -> false
  | Ast.P_tuple (ps, _) -> List.for_all (is_irrefutable_inline env) ps
  | Ast.P_list_exact _ | Ast.P_list_cons _ -> false
  | Ast.P_ctor { name; fields; _ } ->
    ctor_in_single_ctor_type env name
    && (match fields with
        | None -> true
        | Some fs ->
          List.for_all (fun (fp : Ast.field_pat) ->
            match fp.sub with
            | None -> true
            | Some sub -> is_irrefutable_inline env sub) fs)
  | Ast.P_ctor_pos { name; args; _ } ->
    ctor_in_single_ctor_type env name
    && List.for_all (is_irrefutable_inline env) args

and ctor_in_single_ctor_type env name =
  match Types.lookup_ctor env name with
  | None -> false
  | Some info ->
    (match Types.lookup_type env info.owner_type with
     | Some (Types.TI_adt { ctors; _ }) -> List.length ctors = 1
     | _ -> false)
