exception Fatal = Value.Fatal

type capability =
  | Cap_setup
  | Cap_apply
  | Cap_validate
  | Cap_terminal
  | Cap_visibility
  | Cap_action_to_text
  | Cap_text_to_action
  | Cap_view_to_text
  | Cap_outcome_to_text
  | Cap_toplevel

type ctx = {
  link : Link.t;
  builtins : (string, builtin) Hashtbl.t;
  capability : capability;
  roster : Value.player_id list;
  rng : Rng.t ref;
  temp_scope : Pile.scope option;
  toplevel : (string * Value.t) list;
  locals : (string * Value.t) list;
}

and builtin = {
  name : string;
  capabilities : capability list;
  impl : ctx -> Value.t list -> Value.t;
}

(* ---------------------------------------------------------------- *)
(* Context accessors                                                  *)
(* ---------------------------------------------------------------- *)

let ctx_link c = c.link
let ctx_rng c = c.rng
let ctx_temp_scope c = c.temp_scope
let ctx_capability c = c.capability
let ctx_toplevel c = c.toplevel
let ctx_roster c = c.roster

let extend_locals c binds = { c with locals = binds @ c.locals }

let capability_name = function
  | Cap_setup -> "setup"
  | Cap_apply -> "apply"
  | Cap_validate -> "validate"
  | Cap_terminal -> "terminal"
  | Cap_visibility -> "visibility"
  | Cap_action_to_text -> "action_to_text"
  | Cap_text_to_action -> "text_to_action"
  | Cap_view_to_text -> "view_to_text"
  | Cap_outcome_to_text -> "outcome_to_text"
  | Cap_toplevel -> "toplevel"

(* ---------------------------------------------------------------- *)
(* Pattern matching                                                   *)
(* ---------------------------------------------------------------- *)

(* Walks [pat] against runtime [v]. Returns [Some bindings] if the
   pattern matches, [None] if not. Bindings are in reverse encounter
   order — the interpreter prepends them to [ctx.locals] so outer
   bindings are shadowed correctly. *)
let rec match_pattern (pat : Tc_ast.tpattern) (v : Value.t)
    : (string * Value.t) list option =
  match pat.pat_node, v with
  | TP_wild, _ -> Some []
  | TP_var n, _ -> Some [(n, v)]
  | TP_num n, V_num m when n = m -> Some []
  | TP_num _, _ -> None

  | TP_ctor { name; fields; has_rest = _ }, V_ctor c
    when String.equal name c.name ->
    match_ctor_fields fields c.fields

  | TP_ctor _, _ -> None

  | TP_tuple ps, V_tuple vs when List.length ps = List.length vs ->
    match_lists ps vs
  | TP_tuple _, _ -> None

  | TP_list_exact ps, V_list vs when List.length ps = List.length vs ->
    match_lists ps vs
  | TP_list_exact _, _ -> None

  | TP_list_cons { heads; rest }, V_list vs
    when List.length vs >= List.length heads ->
    let h_count = List.length heads in
    let rec split n xs =
      if n = 0 then ([], xs)
      else match xs with
        | [] -> ([], [])
        | x :: xs' ->
          let (a, b) = split (n - 1) xs' in (x :: a, b)
    in
    let head_vs, tail_vs = split h_count vs in
    (match match_lists heads head_vs with
     | None -> None
     | Some binds ->
       match rest with
       | None -> Some binds
       | Some n -> Some ((n, V_list tail_vs) :: binds))
  | TP_list_cons _, _ -> None

and match_ctor_fields
    (pat_fields : (string * Tc_ast.tpattern) list)
    (val_fields : (string * Value.t) list)
    : (string * Value.t) list option =
  List.fold_left (fun acc (fname, fpat) ->
    match acc with
    | None -> None
    | Some binds ->
      (match List.assoc_opt fname val_fields with
       | None -> None
       | Some fv ->
         (match match_pattern fpat fv with
          | None -> None
          | Some b -> Some (b @ binds))))
    (Some []) pat_fields

and match_lists pats vs : (string * Value.t) list option =
  let rec go acc = function
    | [], [] -> Some acc
    | p :: ps, v :: vs ->
      (match match_pattern p v with
       | None -> None
       | Some b -> go (b @ acc) (ps, vs))
    | _ -> None
  in
  go [] (pats, vs)

(* ---------------------------------------------------------------- *)
(* Eval and call                                                      *)
(* ---------------------------------------------------------------- *)

(* [lookup_name] follows the type-system §8.1.2 scoping order: locals
   (lambda params, let-bound, pattern-bound) shadow top-level; top-level
   (fns, top-lets, piles) shadows builtins; stdlib is the bottom of the
   stack. Resolution failures would be a typechecker bug. *)
let lookup_name (c : ctx) (name : string) : Value.t =
  match List.assoc_opt name c.locals with
  | Some v -> v
  | None ->
    (match List.assoc_opt name c.toplevel with
     | Some v -> v
     | None ->
       if Hashtbl.mem c.builtins name then Value.V_builtin name
       else raise (Fatal (Printf.sprintf "unknown name '%s' (typechecker bug)" name)))

let bin_apply op a b =
  let result = match op with
    | Ast.Add -> a + b
    | Ast.Sub -> a - b
    | Ast.Mul -> a * b
    | Ast.Div ->
      if b = 0 then raise (Fatal "division by zero") else a / b
    | Ast.Mod ->
      if b = 0 then raise (Fatal "modulo by zero") else a mod b
  in
  Value.V_num result

let rec eval (c : ctx) (te : Typecheck.texpr) : Value.t =
  match te.node with
  | TE_num n -> V_num n
  | TE_text s -> V_text s
  | TE_var name -> lookup_name c name

  | TE_ctor (name, fields) ->
    let evaled =
      List.map (fun (fname, fe) -> (fname, eval c fe)) fields
    in
    V_ctor { name; fields = evaled }

  | TE_record_update { ctor; spread; fields } ->
    let spread_v = eval c spread in
    (match spread_v with
     | V_ctor cv when String.equal cv.name ctor ->
       let overrides =
         List.map (fun (fname, fe) -> (fname, eval c fe)) fields
       in
       (* Walk declaration-order fields; override if present. *)
       let merged = List.map (fun (fn, fv) ->
         match List.assoc_opt fn overrides with
         | Some v -> (fn, v)
         | None -> (fn, fv)) cv.fields
       in
       V_ctor { name = ctor; fields = merged }
     | _ ->
       raise (Fatal
                (Printf.sprintf
                   "record update: spread value is not a '%s' constructor"
                   ctor)))

  | TE_tuple es -> V_tuple (List.map (eval c) es)
  | TE_list es -> V_list (List.map (eval c) es)

  | TE_app (fn_expr, args) ->
    let fn_val = eval c fn_expr in
    let arg_vals = List.map (eval c) args in
    call c fn_val arg_vals

  | TE_let { pat; value; body } ->
    let value_v = eval c value in
    (match match_pattern pat value_v with
     | Some binds ->
       eval { c with locals = binds @ c.locals } body
     | None ->
       raise (Fatal
                "let: irrefutable pattern failed to match (typechecker bug)"))

  | TE_match { scrutinee; arms } ->
    let scrut_v = eval c scrutinee in
    let rec try_arms = function
      | [] ->
        raise (Fatal
                 "match: no arm matched (exhaustiveness bug)")
      | (pat, body) :: rest ->
        (match match_pattern pat scrut_v with
         | Some binds ->
           eval { c with locals = binds @ c.locals } body
         | None -> try_arms rest)
    in
    try_arms arms

  | TE_lambda { params; body } ->
    V_fn { params; body; captured = c.locals }

  | TE_bin (op, l, r) ->
    (match eval c l, eval c r with
     | V_num a, V_num b -> bin_apply op a b
     | _ ->
       raise (Fatal "binary arithmetic on non-numeric (typechecker bug)"))

  | TE_neg e ->
    (match eval c e with
     | V_num n -> V_num (-n)
     | _ ->
       raise (Fatal "negation on non-numeric (typechecker bug)"))

and call (c : ctx) (fn : Value.t) (args : Value.t list) : Value.t =
  match fn with
  | V_fn closure ->
    let n_params = List.length closure.params in
    let n_args = List.length args in
    if n_params <> n_args then
      raise (Fatal
               (Printf.sprintf
                  "function call: expected %d argument(s), got %d"
                  n_params n_args));
    let param_binds =
      List.map2 (fun (n, _ty) v -> (n, v)) closure.params args
    in
    eval { c with locals = param_binds @ closure.captured } closure.body

  | V_builtin name ->
    (match Hashtbl.find_opt c.builtins name with
     | None ->
       raise (Fatal
                (Printf.sprintf
                   "unknown builtin '%s' (interpreter bug)" name))
     | Some b ->
       if b.capabilities <> []
          && not (List.mem c.capability b.capabilities) then
         raise (Fatal
                  (Printf.sprintf
                     "stdlib '%s' is not available in %s context \
                      (stdlib §16)"
                     name (capability_name c.capability)));
       b.impl c args)

  | V_pile_ctor { name; arity } ->
    if List.length args <> arity then
      raise (Fatal
               (Printf.sprintf
                  "pile '%s': expected %d argument(s), got %d"
                  name arity (List.length args)));
    V_pile_ref { name; keys = args }

  | V_partial { arity; impl } ->
    if List.length args <> arity then
      raise (Fatal
               (Printf.sprintf
                  "partial: expected %d argument(s), got %d"
                  arity (List.length args)));
    impl args

  | _ ->
    raise (Fatal "call: value is not callable (typechecker bug)")

(* ---------------------------------------------------------------- *)
(* Context construction                                               *)
(* ---------------------------------------------------------------- *)

let make_ctx ~link ~builtins ~toplevel ~capability ~roster ~rng ~temp_scope =
  let tbl = Hashtbl.create (List.length builtins + 8) in
  List.iter (fun (b : builtin) -> Hashtbl.replace tbl b.name b) builtins;
  {
    link;
    builtins = tbl;
    capability;
    roster;
    rng;
    temp_scope;
    toplevel;
    locals = [];
  }

(* ---------------------------------------------------------------- *)
(* Top-level environment init                                         *)
(* ---------------------------------------------------------------- *)

(* Extract the set of variable names a typed pattern binds. Mirror
   of [Tc_pass_lets.pat_names] but for [Tc_ast.tpattern]. *)
let rec tpat_names (pat : Tc_ast.tpattern) : string list =
  match pat.pat_node with
  | TP_wild | TP_num _ -> []
  | TP_var n -> [n]
  | TP_ctor { fields; _ } ->
    List.concat_map (fun (_, sub) -> tpat_names sub) fields
  | TP_tuple ps
  | TP_list_exact ps ->
    List.concat_map tpat_names ps
  | TP_list_cons { heads; rest } ->
    let rest_n = match rest with Some n -> [n] | None -> [] in
    List.concat_map tpat_names heads @ rest_n


(* Builds the session-constant top-level env: fn closures, pile
   handles, and evaluated top-level lets. [Typecheck.top_lets]
   returns lets in source order; for evaluation we need topo order.
   Rather than re-exposing [Tc_pass_lets]'s internal sort, we iterate
   a fixpoint: each pass evaluates any let whose free references are
   already bound in [toplevel]. Cycle members (already reported by
   Pass E) remain un-progressable and are silently dropped when the
   pass fails to advance. *)

let free_let_names_in (te : Tc_ast.texpr) : string list =
  (* Gather all free variable names referenced in [te]. We're
     imprecise — we include everything (fns, builtins, piles, lets);
     callers filter by the let-name set. Shadowing is tracked so
     locally-bound names aren't reported. *)
  let rec go shadow acc (e : Tc_ast.texpr) =
    match e.node with
    | TE_num _ | TE_text _ -> acc
    | TE_var n ->
      if List.mem n shadow then acc
      else if List.mem n acc then acc else n :: acc
    | TE_ctor (_, fs) ->
      List.fold_left (fun acc (_, fe) -> go shadow acc fe) acc fs
    | TE_record_update { ctor = _; spread; fields } ->
      let acc = go shadow acc spread in
      List.fold_left (fun acc (_, fe) -> go shadow acc fe) acc fields
    | TE_tuple es | TE_list es ->
      List.fold_left (go shadow) acc es
    | TE_app (f, args) ->
      let acc = go shadow acc f in
      List.fold_left (go shadow) acc args
    | TE_let { pat; value; body } ->
      let acc = go shadow acc value in
      let shadow' = tpat_names pat @ shadow in
      go shadow' acc body
    | TE_match { scrutinee; arms } ->
      let acc = go shadow acc scrutinee in
      List.fold_left (fun acc (pat, body) ->
        let shadow' = tpat_names pat @ shadow in
        go shadow' acc body) acc arms
    | TE_lambda { params; body } ->
      let shadow' = List.map fst params @ shadow in
      go shadow' acc body
    | TE_bin (_, l, r) -> go shadow (go shadow acc l) r
    | TE_neg e -> go shadow acc e
  in
  List.rev (go [] [] te)

let eval_top_lets c lets =
  (* Fixpoint loop: on each pass, evaluate any let whose free refs
     are all resolvable in the current toplevel. Fns and piles were
     pre-populated, so only let-let dependencies can block progress. *)
  let let_names_of_pat (pat : Tc_ast.tpattern) =
    tpat_names pat
  in
  let all_let_names =
    List.concat_map (fun (p, _) -> let_names_of_pat p) lets
  in
  let rec loop remaining toplevel =
    if remaining = [] then toplevel
    else begin
      let progressed, still_blocked, toplevel =
        List.fold_left (fun (progressed, blocked, toplevel) (pat, value) ->
          let free = free_let_names_in value in
          let missing_let_deps =
            List.filter (fun n ->
              List.mem n all_let_names
              && not (List.mem_assoc n toplevel)) free
          in
          if missing_let_deps <> [] then
            (progressed, (pat, value) :: blocked, toplevel)
          else
            let c = { c with toplevel } in
            let v = eval c value in
            match match_pattern pat v with
            | None ->
              raise (Fatal
                       "top-level let: irrefutable pattern failed at init")
            | Some binds ->
              (true, blocked, binds @ toplevel))
          (false, [], toplevel) remaining
      in
      let still_blocked = List.rev still_blocked in
      if not progressed && still_blocked <> [] then
        (* Cycle members remaining. Already reported as type errors
           during Pass E — drop silently. *)
        toplevel
      else
        loop still_blocked toplevel
    end
  in
  loop lets c.toplevel

let build_toplevel (link : Link.t) (builtins : builtin list)
    : (string * Value.t) list =
  let tfile = link.tfile in
  (* Fn closures — params, body, captured=[]. *)
  let fn_entries =
    List.map (fun (name, (texpr : Tc_ast.texpr)) ->
      match texpr.node with
      | TE_lambda { params; body } ->
        (name, Value.V_fn { params; body; captured = [] })
      | _ ->
        (* Pass C wraps every fn body as a TE_lambda. *)
        raise (Fatal
                 (Printf.sprintf
                    "build_toplevel: fn '%s' is not a lambda \
                     (interpreter bug)" name)))
      (Typecheck.top_fns tfile)
  in
  (* Pile handles — nullary → V_pile_ref, parameterized → V_pile_ctor. *)
  let pile_entries =
    List.map (fun (p : Link.pile_info) ->
      match p.key_params with
      | [] -> (p.name, Value.V_pile_ref { name = p.name; keys = [] })
      | ps -> (p.name,
               Value.V_pile_ctor { name = p.name; arity = List.length ps }))
      link.pile_decls
  in
  let initial_toplevel = fn_entries @ pile_entries in
  (* Evaluate top-level lets under Cap_toplevel. *)
  let dummy_rng = ref (Rng.of_seed (Bytes.make 16 '\000')) in
  let c = make_ctx
    ~link ~builtins ~toplevel:initial_toplevel
    ~capability:Cap_toplevel ~roster:[] ~rng:dummy_rng ~temp_scope:None
  in
  eval_top_lets c (Typecheck.top_lets tfile)
