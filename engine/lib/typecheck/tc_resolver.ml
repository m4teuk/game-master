let builtin_nullary = function
  | "Num"      -> Some Types.T_num
  | "Text"     -> Some Types.T_text
  | "PlayerId" -> Some Types.T_player_id
  | "Unit"     -> Some Types.T_unit
  | "RNG"      -> Some Types.T_rng
  | "State"    -> Some Types.T_state
  | "View"     -> Some Types.T_view
  | "Options"  -> Some Types.T_options
  | _ -> None

let builtin_generic_arity = function
  | "List"       -> Some 1
  | "Result"     -> Some 2
  | "GameStatus" -> Some 1
  | "PileView"   -> Some 1
  | "PileRef"    -> Some 1
  | _ -> None

let arity_msg name expected got =
  Printf.sprintf "type '%s' expects %d type argument%s, got %d"
    name expected (if expected = 1 then "" else "s") got

let build_generic name args =
  match name, args with
  | "List", [t]              -> Types.T_list t
  | "Result", [t; e]         -> Types.T_result (t, e)
  | "GameStatus", [r]        -> Types.T_game_status r
  | "PileView", [c]          -> Types.T_pile_view c
  | "PileRef", [c]           -> Types.T_pile_ref c
  | _ -> Types.T_unit

let rec resolve_ty (env : Types.env) (errs : Tc_errors.t)
    (te : Ast.type_expr) : Types.ty =
  match te with
  | Ast.TE_var (name, span) ->
    Tc_errors.report errs span
      (Printf.sprintf
         "type variables are not allowed in user declarations ('%s')"
         name);
    Types.T_unit

  | Ast.TE_tuple (parts, span) ->
    let ts = List.map (resolve_ty env errs) parts in
    if List.length ts < 2 then begin
      Tc_errors.report errs span "tuple type must have at least two components";
      Types.T_unit
    end else
      Types.T_tuple ts

  | Ast.TE_fn (args, ret, _span) ->
    let args' = List.map (resolve_ty env errs) args in
    let ret'  = resolve_ty env errs ret in
    Types.T_fn (args', ret')

  | Ast.TE_app (name, args, span) ->
    let nargs = List.length args in
    (match builtin_nullary name with
     | Some t ->
       if nargs <> 0 then
         Tc_errors.report errs span (arity_msg name 0 nargs);
       t
     | None ->
       (match builtin_generic_arity name with
        | Some expected ->
          if nargs <> expected then begin
            Tc_errors.report errs span (arity_msg name expected nargs);
            let pad = expected - nargs in
            let args =
              if pad > 0 then args @ List.init pad (fun _ ->
                Ast.TE_app ("Unit", [], span))
              else args
            in
            let args' = List.map (resolve_ty env errs) args in
            build_generic name args'
          end else
            build_generic name (List.map (resolve_ty env errs) args)
        | None ->
          (match Types.lookup_type env name with
           | Some _ ->
             if nargs <> 0 then
               Tc_errors.report errs span
                 (Printf.sprintf
                    "user-declared type '%s' is not generic (v0)" name);
             Types.T_user name
           | None ->
             Tc_errors.report errs span
               (Printf.sprintf "unknown type '%s'" name);
             Types.T_unit)))
