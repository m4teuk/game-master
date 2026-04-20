(* ---------------------------------------------------------------- *)
(* Pat-bound name extraction                                          *)
(* ---------------------------------------------------------------- *)

(* Walks a pattern, returning every variable binding it introduces
   along with its source span. Punned fields ([Card { rank }]) bind
   the field name. *)
let rec pat_names (p : Ast.pattern) : (string * Ast.span) list =
  match p with
  | Ast.P_wild _ | Ast.P_num _ -> []
  | Ast.P_var (n, sp) -> [(n, sp)]
  | Ast.P_ctor { fields; _ } ->
    (match fields with
     | None -> []
     | Some fs ->
       List.concat_map (fun (fp : Ast.field_pat) ->
         match fp.sub with
         | None -> [(fp.field, fp.span)]
         | Some sub -> pat_names sub) fs)
  | Ast.P_ctor_pos { args; _ } ->
    List.concat_map pat_names args
  | Ast.P_tuple (ps, _) ->
    List.concat_map pat_names ps
  | Ast.P_list_exact (ps, _) ->
    List.concat_map pat_names ps
  | Ast.P_list_cons { heads; rest; span } ->
    let rest_n = match rest with Some n -> [(n, span)] | None -> [] in
    List.concat_map pat_names heads @ rest_n

(* ---------------------------------------------------------------- *)
(* Collisions                                                         *)
(* ---------------------------------------------------------------- *)

(* Catches three flavours of name clash, each at the shadowing site:
   duplicates within one pattern, collisions against stdlib/piles/fns
   already in [env.values], and collisions against names bound by an
   earlier top-level let. *)

let check_collisions env errs (let_decls : Tc_pass_decls.let_decl list) =
  let _ : string list =
    List.fold_left (fun seen_global (ld : Tc_pass_decls.let_decl) ->
      let pat_ns = pat_names ld.pat in
      let _ : string list =
        List.fold_left (fun s (n, sp) ->
          if List.mem n s then begin
            Tc_errors.report errs sp
              (Printf.sprintf
                 "duplicate variable '%s' in let pattern" n);
            s
          end else n :: s)
          [] pat_ns
      in
      List.fold_left (fun s (n, sp) ->
        if Option.is_some (Types.lookup_value env n) then begin
          Tc_errors.report errs sp
            (Printf.sprintf
               "name '%s' is already a value-level binding \
                (function, pile, let, or stdlib)" n);
          s
        end else if List.mem n s then begin
          Tc_errors.report errs sp
            (Printf.sprintf
               "name '%s' is already bound by another top-level let" n);
          s
        end else n :: s)
        seen_global pat_ns)
      [] let_decls
  in
  ()

(* ---------------------------------------------------------------- *)
(* Irrefutability                                                     *)
(* ---------------------------------------------------------------- *)

(* §6.5: irrefutable iff [_], a [VALUE_IDENT], a tuple of irrefutables,
   or a constructor pattern of a single-constructor type whose subpats
   are all irrefutable. The spec's parenthetical "(i.e. a record)" is
   loose — the operative property is single-constructor, not the
   record-name-equals-type-name rule of §3.1. *)

let ctor_in_single_ctor_type env name =
  match Types.lookup_ctor env name with
  | None -> false
  | Some info ->
    (match Types.lookup_type env info.owner_type with
     | Some (Types.TI_adt { ctors; _ }) -> List.length ctors = 1
     | _ -> false)

let rec pat_irrefutable env (p : Ast.pattern) : bool =
  match p with
  | Ast.P_wild _ | Ast.P_var _ -> true
  | Ast.P_num _ -> false
  | Ast.P_tuple (ps, _) -> List.for_all (pat_irrefutable env) ps
  | Ast.P_list_exact _ | Ast.P_list_cons _ -> false
  | Ast.P_ctor { name; fields; _ } ->
    ctor_in_single_ctor_type env name
    && (match fields with
        | None -> true
        | Some fs ->
          List.for_all (fun (fp : Ast.field_pat) ->
            match fp.sub with
            | None -> true
            | Some sub -> pat_irrefutable env sub) fs)
  | Ast.P_ctor_pos { name; args; _ } ->
    ctor_in_single_ctor_type env name
    && List.for_all (pat_irrefutable env) args

let check_irrefutability env errs (let_decls : Tc_pass_decls.let_decl list) =
  List.iter (fun (ld : Tc_pass_decls.let_decl) ->
    if not (pat_irrefutable env ld.pat) then
      Tc_errors.report errs (Ast.span_of_pattern ld.pat)
        "let-binding pattern must be irrefutable (type-system §6.5)")
    let_decls

(* ---------------------------------------------------------------- *)
(* Dependency analysis                                                *)
(* ---------------------------------------------------------------- *)

(* Returns the names from [let_names] that appear free in [e],
   excluding those locally shadowed by enclosing binders. Lambda
   params, inner [let] pats, and [match]-arm pats all extend the
   shadow set when descending into their body. *)

let add_unique x xs = if List.mem x xs then xs else x :: xs
let union_sets a b = List.fold_left (fun acc x -> add_unique x acc) a b

let rec free_let_refs (let_names : string list)
    (shadowed : string list) (e : Ast.expr) : string list =
  let go = free_let_refs let_names shadowed in
  match e with
  | Ast.E_num _ | Ast.E_text _ | Ast.E_ctor _ -> []
  | Ast.E_var (n, _) ->
    if List.mem n shadowed then []
    else if List.mem n let_names then [n]
    else []
  | Ast.E_record { body = { spread; fields }; _ } ->
    let s0 = match spread with None -> [] | Some e -> go e in
    List.fold_left (fun acc (_, e) -> union_sets acc (go e)) s0 fields
  | Ast.E_tuple (es, _) | Ast.E_list (es, _) ->
    List.fold_left (fun acc e -> union_sets acc (go e)) [] es
  | Ast.E_paren (e, _) -> go e
  | Ast.E_app (f, args, _) ->
    let fs = go f in
    List.fold_left (fun acc a ->
      let e = match a with Ast.A_pos e | Ast.A_kw (_, e) -> e in
      union_sets acc (go e)) fs args
  | Ast.E_let { pat; value; body; _ } ->
    let v = go value in
    let new_shadow =
      List.fold_left (fun s (n, _) -> add_unique n s)
        shadowed (pat_names pat)
    in
    let b = free_let_refs let_names new_shadow body in
    union_sets v b
  | Ast.E_match { scrutinee; arms; _ } ->
    let s0 = go scrutinee in
    List.fold_left (fun acc (p, e) ->
      let new_shadow =
        List.fold_left (fun s (n, _) -> add_unique n s)
          shadowed (pat_names p)
      in
      union_sets acc (free_let_refs let_names new_shadow e))
      s0 arms
  | Ast.E_lambda { params; body; _ } ->
    let new_shadow =
      List.fold_left (fun s (p : Ast.param) -> add_unique p.name s)
        shadowed params
    in
    free_let_refs let_names new_shadow body
  | Ast.E_bin (_, l, r, _) -> union_sets (go l) (go r)
  | Ast.E_neg (e, _) -> go e

(* ---------------------------------------------------------------- *)
(* Topological sort with cycle detection                              *)
(* ---------------------------------------------------------------- *)

(* Each let is one node. If its pattern binds multiple names they
   share a single dependency set (the RHS's). Edge [i -> j] means
   "let i references a name bound by let j", so j must be processed
   before i.

   Standard 3-color DFS. On a back-edge to a [Gray] node we report a
   cycle on that node (deduplicated via [cycle_reported]). The
   returned list is in dependency-first order: deps before
   dependents. *)

let topo_sort errs (let_decls : Tc_pass_decls.let_decl list) =
  let n = List.length let_decls in
  let arr = Array.of_list let_decls in
  let name_to_idx = Hashtbl.create (max 1 n) in
  Array.iteri (fun i (ld : Tc_pass_decls.let_decl) ->
    List.iter (fun (name, _) -> Hashtbl.replace name_to_idx name i)
      (pat_names ld.pat)) arr;
  let let_names = Hashtbl.fold (fun k _ acc -> k :: acc) name_to_idx [] in
  let deps = Array.make n [] in
  Array.iteri (fun i (ld : Tc_pass_decls.let_decl) ->
    let refs = free_let_refs let_names [] ld.body in
    let dep_idxs =
      List.filter_map (Hashtbl.find_opt name_to_idx) refs
      |> List.sort_uniq compare
    in
    deps.(i) <- dep_idxs) arr;
  let white = 0 and gray = 1 and black = 2 in
  let color = Array.make n white in
  let cycle_reported = Array.make n false in
  let topo = ref [] in
  let rec dfs i =
    if color.(i) = black then ()
    else if color.(i) = gray then begin
      if not cycle_reported.(i) then begin
        cycle_reported.(i) <- true;
        let ld = arr.(i) in
        let nm =
          match pat_names ld.pat with
          | (n, _) :: _ -> n
          | [] -> "<unbound>"
        in
        Tc_errors.report errs ld.span
          (Printf.sprintf
             "let '%s' participates in a value-level cycle \
              (depends transitively on itself)" nm)
      end
    end else begin
      color.(i) <- gray;
      List.iter dfs deps.(i);
      color.(i) <- black;
      topo := i :: !topo
    end
  in
  for i = 0 to n - 1 do dfs i done;
  (* [!topo] is in reverse finish order; reverse to get dependency-first. *)
  List.rev_map (fun i -> arr.(i)) !topo

(* ---------------------------------------------------------------- *)
(* Inference                                                          *)
(* ---------------------------------------------------------------- *)

(* Walk the topo-ordered lets, type-checking each body, destructuring
   against the pattern to derive variable types, and extending the env
   so later lets (and Passes B/C/D) see them. *)

let infer_let env errs (ld : Tc_pass_decls.let_decl)
    : Types.env * (Tc_ast.tpattern * Tc_ast.texpr) =
  let value_t = Tc_expr.check_expr env errs ~expected:None ld.body in
  let tpat, bindings =
    Tc_expr.check_pattern env errs ~expected:value_t.ty ld.pat
  in
  let env =
    List.fold_left (fun e (n, t) -> Types.add_value e n t) env bindings
  in
  (env, (tpat, value_t))

(* The structural pass returns lets in topo order; we infer in that
   order. After inference completes, sort the typed entries back into
   source order so [top_lets] reads naturally for downstream consumers
   (and matches what a user would expect from [let_decls] in declarative
   order). *)

let run env errs (let_decls : Tc_pass_decls.let_decl list)
    : Types.env * (Tc_ast.tpattern * Tc_ast.texpr) list =
  check_collisions env errs let_decls;
  check_irrefutability env errs let_decls;
  let ordered = topo_sort errs let_decls in
  let env, typed_in_topo =
    List.fold_left (fun (env, acc) ld ->
      let env, entry = infer_let env errs ld in
      (env, (ld, entry) :: acc))
      (env, []) ordered
  in
  let typed_in_topo = List.rev typed_in_topo in
  let in_source_order =
    List.filter_map (fun ld_src ->
      List.find_map (fun (ld_topo, entry) ->
        if ld_topo == ld_src then Some entry else None)
        typed_in_topo)
      let_decls
  in
  (env, in_source_order)
