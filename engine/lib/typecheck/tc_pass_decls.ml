type pile_decl = {
  name : string;
  params : (string * Types.ty) list;
  card_ty : Types.ty;
  visibility : Ast.expr;
  span : Ast.span;
}

type options_field = {
  name : string;
  ty : Types.ty;
  default : Ast.expr;
  span : Ast.span;
}

type fn_decl = {
  name : string;
  params : (string * Types.ty) list;
  ret : Types.ty;
  body : Ast.expr;
  span : Ast.span;
}

type let_decl = {
  pat : Ast.pattern;
  body : Ast.expr;
  span : Ast.span;
}

type result = {
  env : Types.env;
  type_decls : (string * Types.type_info) list;
  pile_decls : pile_decl list;
  options_decl : options_field list;
  fn_decls : fn_decl list;
  let_decls : let_decl list;
}

(* ---------------------------------------------------------------- *)
(* A1 — type declarations                                            *)
(* ---------------------------------------------------------------- *)

(* Type names reserved by the engine. The algebraic built-ins
   (Visibility, Ordering, Flag, Result, GameStatus, PileView) are also
   in [env.types] via [Builtins.seed_types], so [lookup_type] catches
   them; this list covers the non-ADT built-ins which aren't seeded. *)
let builtin_type_names = [
  "Num"; "Text"; "PlayerId"; "Unit"; "RNG"; "State"; "View"; "Options";
  "List"; "Result"; "GameStatus"; "PileView"; "PileRef";
]

let is_builtin_type_name name = List.mem name builtin_type_names

(* Constructor names reserved by the engine but not seeded into
   [env.ctors] by [Builtins] (because they're synthesized later in
   Pass A — currently just [Options], from the [options { }] block). *)
let builtin_ctor_names = [ "Options" ]
let is_builtin_ctor_name name = List.mem name builtin_ctor_names

(* Type-system §1.1: State, View, RNG, and PileRef<C> cannot be stored
   inside user-declared types. Recurses into containers so e.g.
   List<State> is also rejected. Stops at T_user to avoid cycles —
   opaqueness inside another user type is checked at that type's
   declaration. *)
let rec contains_opaque (ty : Types.ty) =
  match ty with
  | Types.T_state | Types.T_view | Types.T_rng -> true
  | Types.T_pile_ref _ -> true
  | Types.T_num | Types.T_text | Types.T_player_id
  | Types.T_unit | Types.T_options -> false
  | Types.T_list t
  | Types.T_pile_view t
  | Types.T_game_status t -> contains_opaque t
  | Types.T_result (a, b) -> contains_opaque a || contains_opaque b
  | Types.T_tuple ts -> List.exists contains_opaque ts
  | Types.T_fn (args, ret) ->
    List.exists contains_opaque args || contains_opaque ret
  | Types.T_user _ -> false
  | Types.T_var _ -> false

type type_spec = {
  spec_name : string;
  spec_ctors : Ast.ctor_decl list;
  spec_span : Ast.span;
}

(* Sweep 1: register each user type name with a placeholder so that
   subsequent ctor-field resolution can see all type names (supporting
   mutual recursion, type-system §3). *)
let collect_type_names env errs decls =
  let env, rev_specs =
    List.fold_left (fun (env, specs) decl ->
      match decl with
      | Ast.D_type { name; ctors; span } ->
        if is_builtin_type_name name then begin
          Tc_errors.report errs span
            (Printf.sprintf "type name '%s' is reserved by the engine" name);
          (env, specs)
        end else (match Types.lookup_type env name with
          | Some _ ->
            Tc_errors.report errs span
              (Printf.sprintf "type '%s' is already declared" name);
            (env, specs)
          | None ->
            let placeholder =
              Types.TI_adt { ctors = []; is_record = false } in
            let env = Types.add_type env name placeholder in
            (env, { spec_name = name; spec_ctors = ctors;
                    spec_span = span } :: specs))
      | _ -> (env, specs))
      (env, []) decls
  in
  (env, List.rev rev_specs)

let resolve_fields env errs ctor_name fields =
  let _, rev =
    List.fold_left (fun (seen, acc) (fd : Ast.field_decl) ->
      let fname = fd.name in
      let ty = fd.ty in
      let span = fd.span in
      if List.mem fname seen then begin
        Tc_errors.report errs span
          (Printf.sprintf
             "duplicate field '%s' in constructor '%s'" fname ctor_name);
        (seen, acc)
      end else begin
        let ty = Tc_resolver.resolve_ty env errs ty in
        if contains_opaque ty then
          Tc_errors.report errs span
            (Printf.sprintf
               "field '%s.%s' has opaque type '%s' (State, View, RNG, \
                and PileRef cannot appear in user-declared types)"
               ctor_name fname (Types.string_of_ty ty));
        (fname :: seen, (fname, ty) :: acc)
      end)
      ([], []) fields
  in
  List.rev rev

(* Sweep 2: resolve each type's constructors, threading env so later
   types see earlier types' ctors when checking ctor-name uniqueness. *)
let resolve_one_type env errs spec =
  let _, rev_ctors =
    List.fold_left (fun (seen_local, acc) (ctor : Ast.ctor_decl) ->
      let { Ast.name = cname; fields; span = cspan } = ctor in
      if is_builtin_ctor_name cname then begin
        Tc_errors.report errs cspan
          (Printf.sprintf
             "constructor name '%s' is reserved by the engine" cname);
        (cname :: seen_local, acc)
      end else if Option.is_some (Types.lookup_ctor env cname)
                  || List.mem cname seen_local then begin
        Tc_errors.report errs cspan
          (Printf.sprintf "constructor '%s' is already declared" cname);
        (cname :: seen_local, acc)
      end else
        let resolved_fields = match fields with
          | None | Some [] -> []
          | Some fs -> resolve_fields env errs cname fs
        in
        let info : Types.ctor_info = {
          ctor_name = cname;
          owner_type = spec.spec_name;
          fields = resolved_fields;
          is_record = false;
        } in
        (cname :: seen_local, info :: acc))
      ([], []) spec.spec_ctors
  in
  let ctor_infos = List.rev rev_ctors in
  let is_record =
    match ctor_infos with
    | [c] when String.equal c.Types.ctor_name spec.spec_name -> true
    | _ -> false
  in
  let ctor_infos =
    List.map (fun (i : Types.ctor_info) -> { i with is_record }) ctor_infos
  in
  let type_info = Types.TI_adt { ctors = ctor_infos; is_record } in
  (type_info, ctor_infos)

let resolve_type_ctors env errs specs =
  let env, rev_decls =
    List.fold_left (fun (env, type_decls) spec ->
      let type_info, ctor_infos = resolve_one_type env errs spec in
      let env = Types.add_type env spec.spec_name type_info in
      let env =
        List.fold_left (fun env (info : Types.ctor_info) ->
          Types.add_ctor env info.ctor_name info)
          env ctor_infos
      in
      (env, (spec.spec_name, type_info) :: type_decls))
      (env, []) specs
  in
  (env, List.rev rev_decls)

(* ---------------------------------------------------------------- *)
(* A2 — pile declarations                                            *)
(* ---------------------------------------------------------------- *)

(* Pile names live in [env.values] alongside functions and top-level
   lets. The spec (§8.1.2) loosely calls them "constructors of
   PileRef<C>", but pile names never appear in patterns (PileRef is
   opaque per §1.1) and the parser treats [hand(alice)] as an [E_app]
   on a value identifier — so the value namespace is the right home. *)

let resolve_pile_params env errs pile_name (params : Ast.field_decl list) =
  let _, rev =
    List.fold_left (fun (seen, acc) (fd : Ast.field_decl) ->
      let pname = fd.name in
      let span = fd.span in
      if List.mem pname seen then begin
        Tc_errors.report errs span
          (Printf.sprintf
             "duplicate parameter '%s' on pile '%s'" pname pile_name);
        (seen, acc)
      end else begin
        let ty = Tc_resolver.resolve_ty env errs fd.ty in
        if contains_opaque ty then
          Tc_errors.report errs span
            (Printf.sprintf
               "pile parameter '%s.%s' has opaque type '%s'"
               pile_name pname (Types.string_of_ty ty));
        (pname :: seen, (pname, ty) :: acc)
      end)
      ([], []) params
  in
  List.rev rev

let collect_piles env errs decls =
  let env, rev_piles =
    List.fold_left (fun (env, acc) decl ->
      match decl with
      | Ast.D_pile { name; params; card_ty; visibility; span } ->
        if Option.is_some (Types.lookup_value env name) then begin
          Tc_errors.report errs span
            (Printf.sprintf
               "name '%s' is already a value-level binding \
                (pile, function, let, or stdlib)" name);
          (env, acc)
        end else begin
          let resolved_params = resolve_pile_params env errs name params in
          let resolved_card = Tc_resolver.resolve_ty env errs card_ty in
          if contains_opaque resolved_card then
            Tc_errors.report errs (Ast.span_of_type_expr card_ty)
              (Printf.sprintf
                 "pile '%s' has opaque card type '%s'"
                 name (Types.string_of_ty resolved_card));
          let pile_value_ty =
            let ref_ty = Types.T_pile_ref resolved_card in
            match resolved_params with
            | [] -> ref_ty
            | ps -> Types.T_fn (List.map snd ps, ref_ty)
          in
          let env = Types.add_value env name pile_value_ty in
          let env =
            if List.length resolved_params > 0 then
              Types.add_param_names env name (List.map fst resolved_params)
            else env
          in
          let pd : pile_decl = {
            name;
            params = resolved_params;
            card_ty = resolved_card;
            visibility;
            span;
          } in
          (env, pd :: acc)
        end
      | _ -> (env, acc))
      (env, []) decls
  in
  (env, List.rev rev_piles)

(* ---------------------------------------------------------------- *)
(* A3 — options block                                                *)
(* ---------------------------------------------------------------- *)

(* Per type-system §9: only Num, Text, and all-nullary algebraic types
   (user enums or built-in [Flag]/[Visibility]/[Ordering]) are valid
   options field types. The all-nullary check uses the env, which by
   A3 has all user types resolved. *)

let is_all_nullary_adt env name =
  match Types.lookup_type env name with
  | Some (Types.TI_adt { ctors; _ }) ->
    List.for_all (fun c -> List.length c.Types.fields = 0) ctors
  | _ -> false

let valid_options_field_type env ty =
  match ty with
  | Types.T_num | Types.T_text -> true
  | Types.T_user name -> is_all_nullary_adt env name
  | _ -> false

let resolve_options_fields env errs (fields : Ast.option_field list)
    : options_field list =
  let _, rev =
    List.fold_left (fun (seen, acc) (of_ : Ast.option_field) ->
      let fname = of_.name in
      let span = of_.span in
      if List.mem fname seen then begin
        Tc_errors.report errs span
          (Printf.sprintf "duplicate options field '%s'" fname);
        (seen, acc)
      end else begin
        let ty = Tc_resolver.resolve_ty env errs of_.ty in
        if not (valid_options_field_type env ty) then
          Tc_errors.report errs span
            (Printf.sprintf
               "options field '%s' has type '%s'; only Num, Text, and \
                all-nullary algebraic types are allowed (type-system §9)"
               fname (Types.string_of_ty ty));
        let resolved : options_field = {
          name = fname;
          ty;
          default = of_.default;
          span;
        } in
        (fname :: seen, resolved :: acc)
      end)
      ([], []) fields
  in
  List.rev rev

let synthesize_options_type env (fields : options_field list) =
  let ctor_fields =
    List.map (fun (f : options_field) -> (f.name, f.ty)) fields
  in
  let info : Types.ctor_info = {
    ctor_name = "Options";
    owner_type = "Options";
    fields = ctor_fields;
    is_record = true;
  } in
  let env =
    Types.add_type env "Options"
      (Types.TI_adt { ctors = [info]; is_record = true })
  in
  Types.add_ctor env "Options" info

let collect_options env errs decls =
  let blocks =
    List.filter_map (function
      | Ast.D_options { fields; span } -> Some (fields, span)
      | _ -> None)
      decls
  in
  match blocks with
  | [] ->
    (env |> synthesize_options_type) [], []
  | (fields, _span) :: rest ->
    List.iter (fun (_, sp) ->
      Tc_errors.report errs sp
        "multiple 'options' declarations are not allowed (type-system §10)")
      rest;
    let resolved = resolve_options_fields env errs fields in
    let env = synthesize_options_type env resolved in
    env, resolved

(* ---------------------------------------------------------------- *)
(* A4 — function signatures                                          *)
(* ---------------------------------------------------------------- *)

(* Top-level function declarations register their signature in
   [env.values] so that other declarations (and bodies in Pass C) can
   call them in any order (forward refs allowed by §8.3). Bodies
   themselves are deferred. Per §5, every parameter and the return
   type must be annotated; missing annotations are reported and a
   [T_unit] placeholder substituted so the rest of the signature can
   still register. *)

let resolve_fn_params env errs fn_name (params : Ast.param list) =
  let _, rev =
    List.fold_left (fun (seen, acc) (p : Ast.param) ->
      let pname = p.name in
      let span = p.span in
      if List.mem pname seen then begin
        Tc_errors.report errs span
          (Printf.sprintf
             "duplicate parameter '%s' in function '%s'" pname fn_name);
        (seen, acc)
      end else begin
        let ty = match p.annot with
          | Some te -> Tc_resolver.resolve_ty env errs te
          | None ->
            Tc_errors.report errs span
              (Printf.sprintf
                 "parameter '%s' of function '%s' must have a type \
                  annotation (type-system §5)" pname fn_name);
            Types.T_unit
        in
        (pname :: seen, (pname, ty) :: acc)
      end)
      ([], []) params
  in
  List.rev rev

let collect_fns env errs decls =
  let env, rev_fns =
    List.fold_left (fun (env, acc) decl ->
      match decl with
      | Ast.D_fn { name; params; ret; body; span } ->
        if Option.is_some (Types.lookup_value env name) then begin
          Tc_errors.report errs span
            (Printf.sprintf
               "name '%s' is already a value-level binding \
                (function, pile, let, or stdlib)" name);
          (env, acc)
        end else begin
          let resolved_params = resolve_fn_params env errs name params in
          let resolved_ret = Tc_resolver.resolve_ty env errs ret in
          let fn_ty =
            Types.T_fn (List.map snd resolved_params, resolved_ret)
          in
          let env = Types.add_value env name fn_ty in
          let env =
            Types.add_param_names env name (List.map fst resolved_params)
          in
          let fd : fn_decl = {
            name;
            params = resolved_params;
            ret = resolved_ret;
            body;
            span;
          } in
          (env, fd :: acc)
        end
      | _ -> (env, acc))
      (env, []) decls
  in
  (env, List.rev rev_fns)

(* ---------------------------------------------------------------- *)
(* A5 — top-level lets                                               *)
(* ---------------------------------------------------------------- *)

(* Pure collection. Lets are not annotated, so their types must be
   inferred from the RHS. Inference, value-cycle detection, and
   collision against stdlib/piles/fns all live in Pass E
   ([Tc_pass_exprs]) — that pass needs to topo-sort lets by their
   dependencies on each other before it can extend [env.values]. *)

let collect_lets decls =
  List.filter_map (function
    | Ast.D_let { pat; body; span } ->
      Some { pat; body; span }
    | _ -> None)
    decls

(* ---------------------------------------------------------------- *)
(* Entry                                                             *)
(* ---------------------------------------------------------------- *)

(* Inject a default type declaration if the user didn't write one.
   These are the types that almost every ruleset would declare
   identically:

   - [PlayerDict = PlayerDict { }] — the empty per-player record,
     used by games with no per-player public state.
   - [Suit = Clubs | Diamonds | Hearts | Spades] — standard 4-suit deck.
   - [Card = Card { suit: Suit, rank: Num }] — the shape assumed by
     the stdlib card helpers ([fresh_deck], [card_rank], etc.).

   User declarations always win — we only inject when the name is
   absent. Injection happens after [collect_type_names] (which adds
   placeholders for every user `type X = …`) so [lookup_type "X"]
   already returns [Some] for user-declared X, and the user's
   declaration carries through unchanged. *)

let inject_default_record env type_decls ~name ~fields =
  (* Only inject if neither the type name nor its constructor name is
     already taken. Skipping silently if the constructor exists is the
     least-surprising behavior — a user who declared
     `type Color = Card | …` should keep their `Card` ctor. *)
  if Option.is_some (Types.lookup_type env name)
     || Option.is_some (Types.lookup_ctor env name)
  then (env, type_decls)
  else
    let ctor = { Types.ctor_name = name;
                 owner_type = name;
                 fields;
                 is_record = true } in
    let ti = Types.TI_adt { ctors = [ctor]; is_record = true } in
    let env = Types.add_type env name ti in
    let env = Types.add_ctor env name ctor in
    (env, type_decls @ [(name, ti)])

let inject_default_enum env type_decls ~name ~ctor_names =
  let ctor_collision = List.exists (fun n ->
    Option.is_some (Types.lookup_ctor env n)) ctor_names
  in
  if Option.is_some (Types.lookup_type env name) || ctor_collision
  then (env, type_decls)
  else
    let ctors = List.map (fun n ->
      { Types.ctor_name = n; owner_type = name;
        fields = []; is_record = false }) ctor_names in
    let ti = Types.TI_adt { ctors; is_record = false } in
    let env = Types.add_type env name ti in
    let env = List.fold_left (fun e (c : Types.ctor_info) ->
      Types.add_ctor e c.ctor_name c) env ctors in
    (env, type_decls @ [(name, ti)])

let inject_defaults env type_decls =
  let (env, type_decls) =
    inject_default_record env type_decls ~name:"PlayerDict" ~fields:[]
  in
  let (env, type_decls) =
    inject_default_enum env type_decls ~name:"Suit"
      ~ctor_names:["Clubs"; "Diamonds"; "Hearts"; "Spades"]
  in
  let (env, type_decls) =
    inject_default_record env type_decls ~name:"Card"
      ~fields:[("suit", Types.T_user "Suit"); ("rank", Types.T_num)]
  in
  (env, type_decls)

let run (env : Types.env) (errs : Tc_errors.t) (file : Ast.file) : result =
  let env, specs = collect_type_names env errs file.decls in
  (* Inject defaults BEFORE resolving user types' field types — this
     way user code that mentions Card/Suit/PlayerDict in its own type
     declarations (e.g. `type Outcome = Win { c: Card }`) sees the
     injected names too. User declarations always still win because
     [collect_type_names] runs first and registers a placeholder. *)
  let env, type_decls_default = inject_defaults env [] in
  let env, type_decls = resolve_type_ctors env errs specs in
  let type_decls = type_decls @ type_decls_default in
  let env, pile_decls = collect_piles env errs file.decls in
  let env, options_decl = collect_options env errs file.decls in
  let env, fn_decls = collect_fns env errs file.decls in
  let let_decls = collect_lets file.decls in
  {
    env;
    type_decls;
    pile_decls;
    options_decl;
    fn_decls;
    let_decls;
  }
