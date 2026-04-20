(* Per-type case analysis. v0 keeps it shallow: only the top-level
   pattern shape is examined per scrutinee type. Nested exhaustiveness
   (e.g. checking that a tuple's components are themselves covered)
   would need Maranget-style matrix reduction; left for a future
   iteration. *)

let owner_of_ty (ty : Types.ty) : string option =
  match ty with
  | Types.T_user name -> Some name
  | Types.T_options -> Some "Options"
  | Types.T_result _ -> Some "Result"
  | Types.T_game_status _ -> Some "GameStatus"
  | Types.T_pile_view _ -> Some "PileView"
  | _ -> None

let check_adt_coverage env errs span ty patterns =
  let covered_names =
    List.filter_map (function
      | Ast.P_ctor { name; _ } -> Some name
      | Ast.P_ctor_pos { name; _ } -> Some name
      | _ -> None) patterns
  in
  match owner_of_ty ty with
  | None -> ()
  | Some owner ->
    (match Types.lookup_type env owner with
     | Some (Types.TI_adt { ctors; _ }) ->
       let missing =
         List.filter_map (fun (c : Types.ctor_info) ->
           if List.mem c.ctor_name covered_names then None
           else Some c.ctor_name) ctors
       in
       if missing <> [] then
         Tc_errors.report errs span
           (Printf.sprintf
              "non-exhaustive match on '%s'; missing constructor(s): %s"
              (Types.string_of_ty ty)
              (String.concat ", " missing))
     | _ -> ())

let check (env : Types.env) (errs : Tc_errors.t)
    ~(scrutinee_ty : Types.ty) ~(scrutinee_span : Ast.span)
    (patterns : Ast.pattern list) : unit =
  if List.length patterns = 0 then begin
    Tc_errors.report errs scrutinee_span
      "match expression has no arms (always non-exhaustive)";
    ()
  end else begin
    (* Reachability / unreachable-arm detection (type-system §6.3) is a
       warning category, not an error, and v0 doesn't yet have a
       warning surface — defer until [Tc_errors] grows one. *)
    let has_catchall = List.exists (function
      | Ast.P_wild _ | Ast.P_var _ -> true
      | _ -> false) patterns
    in
    if has_catchall then ()
    else match scrutinee_ty with
      | Types.T_state | Types.T_view | Types.T_rng | Types.T_pile_ref _
      | Types.T_fn _ ->
        (* Already reported by [Tc_expr.check_match] at the scrutinee. *)
        ()
      | Types.T_num | Types.T_text
      | Types.T_unit | Types.T_player_id ->
        Tc_errors.report errs scrutinee_span
          (Printf.sprintf
             "non-exhaustive match on '%s' (must end in a wildcard or \
              value-binding pattern)"
             (Types.string_of_ty scrutinee_ty))
      | Types.T_list _ ->
        let has_empty = List.exists (function
          | Ast.P_list_exact ([], _) -> true | _ -> false) patterns in
        let has_cons = List.exists (function
          | Ast.P_list_cons _ -> true | _ -> false) patterns in
        if not has_empty then
          Tc_errors.report errs scrutinee_span
            "non-exhaustive list match: missing empty-list case '[]'";
        if not has_cons then
          Tc_errors.report errs scrutinee_span
            "non-exhaustive list match: missing non-empty case \
             (use '[x, ..xs]' or a wildcard)"
      | Types.T_tuple _ ->
        let has_tuple_pat = List.exists (function
          | Ast.P_tuple _ -> true | _ -> false) patterns in
        if not has_tuple_pat then
          Tc_errors.report errs scrutinee_span
            "non-exhaustive tuple match (use a tuple pattern or wildcard)"
      | Types.T_user _ | Types.T_options | Types.T_result _
      | Types.T_game_status _ | Types.T_pile_view _ ->
        check_adt_coverage env errs scrutinee_span scrutinee_ty patterns
      | Types.T_var _ ->
        Tc_errors.report errs scrutinee_span
          "cannot check exhaustiveness on an unresolved type variable"
  end
