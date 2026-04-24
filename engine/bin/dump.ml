(* Pretty-printers for the dev CLI. Aim: dense, parseable, span-tagged
   output — not aesthetic. Spans are shown as [line:col-line:col] so
   you can cross-reference back to the source. *)

open Engine.Dev

let pp_span (sp : Load_error.span) =
  Printf.sprintf "%d:%d-%d:%d"
    sp.start_line sp.start_col sp.end_line sp.end_col

(* ---------------------------------------------------------------- *)
(* Tokens                                                             *)
(* ---------------------------------------------------------------- *)

let token_repr (t : Token.t) : string =
  match t with
  | TYPE_IDENT s -> Printf.sprintf "TYPE_IDENT %S" s
  | VALUE_IDENT s -> Printf.sprintf "VALUE_IDENT %S" s
  | NUM_LIT n -> Printf.sprintf "NUM_LIT %d" n
  | TEXT_LIT s -> Printf.sprintf "TEXT_LIT %S" s
  | t -> Token.to_string t

let dump_tokens fmt (toks : Token.positioned list) =
  List.iter (fun (p : Token.positioned) ->
    Format.fprintf fmt "%-18s %s@." (pp_span p.span) (token_repr p.token))
    toks

(* ---------------------------------------------------------------- *)
(* AST                                                                *)
(* ---------------------------------------------------------------- *)

let pp_comma_list pp fmt items =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
    pp fmt items

let rec pp_type_expr fmt (te : Ast.type_expr) =
  match te with
  | Ast.TE_app (name, [], _) -> Format.fprintf fmt "%s" name
  | Ast.TE_app (name, args, _) ->
    Format.fprintf fmt "%s<@[%a@]>" name (pp_comma_list pp_type_expr) args
  | Ast.TE_tuple (parts, _) ->
    Format.fprintf fmt "(@[%a@])" (pp_comma_list pp_type_expr) parts
  | Ast.TE_fn (args, ret, _) ->
    Format.fprintf fmt "(@[%a@]) -> %a"
      (pp_comma_list pp_type_expr) args pp_type_expr ret
  | Ast.TE_var (name, _) -> Format.fprintf fmt "%s" name

let rec pp_pattern fmt (p : Ast.pattern) =
  match p with
  | Ast.P_wild _ -> Format.fprintf fmt "_"
  | Ast.P_var (n, _) -> Format.fprintf fmt "%s" n
  | Ast.P_num (n, _) -> Format.fprintf fmt "%d" n
  | Ast.P_ctor { name; fields; has_rest; _ } ->
    (match fields with
     | None -> Format.fprintf fmt "%s" name
     | Some fs ->
       Format.fprintf fmt "%s {@[%a%s@]}"
         name (pp_comma_list pp_field_pat) fs
         (if has_rest then ", .." else ""))
  | Ast.P_ctor_pos { name; args; _ } ->
    Format.fprintf fmt "%s(@[%a@])" name (pp_comma_list pp_pattern) args
  | Ast.P_tuple (ps, _) ->
    Format.fprintf fmt "(@[%a@])" (pp_comma_list pp_pattern) ps
  | Ast.P_list_exact (ps, _) ->
    Format.fprintf fmt "[@[%a@]]" (pp_comma_list pp_pattern) ps
  | Ast.P_list_cons { heads; rest; _ } ->
    let rest_str = match rest with Some n -> ", .." ^ n | None -> ", .." in
    Format.fprintf fmt "[@[%a%s@]]" (pp_comma_list pp_pattern) heads rest_str

and pp_field_pat fmt (fp : Ast.field_pat) =
  match fp.sub with
  | None -> Format.fprintf fmt "%s" fp.field
  | Some p -> Format.fprintf fmt "%s: %a" fp.field pp_pattern p

let bin_op_str = function
  | Ast.Add -> "+" | Ast.Sub -> "-" | Ast.Mul -> "*"
  | Ast.Div -> "/" | Ast.Mod -> "mod"

let rel_op_str = function
  | Ast.RLt -> "<" | Ast.RLte -> "<=" | Ast.RGt -> ">" | Ast.RGte -> ">="
  | Ast.REq -> "==" | Ast.RNeq -> "!="

let rec pp_expr fmt (e : Ast.expr) =
  match e with
  | Ast.E_num (n, _) -> Format.fprintf fmt "%d" n
  | Ast.E_text (s, _) -> Format.fprintf fmt "%S" s
  | Ast.E_var (n, _) -> Format.fprintf fmt "%s" n
  | Ast.E_ctor (n, _) -> Format.fprintf fmt "%s" n
  | Ast.E_record { ctor; body = { spread; fields }; _ } ->
    let spread_part fmt = match spread with
      | None -> ()
      | Some e -> Format.fprintf fmt "..%a, " pp_expr e
    in
    Format.fprintf fmt "%s {@[%t%a@]}" ctor spread_part
      (pp_comma_list (fun fmt (n, e) ->
         Format.fprintf fmt "%s: %a" n pp_expr e))
      fields
  | Ast.E_tuple (es, _) ->
    Format.fprintf fmt "(@[%a@])" (pp_comma_list pp_expr) es
  | Ast.E_list (es, _) ->
    Format.fprintf fmt "[@[%a@]]" (pp_comma_list pp_expr) es
  | Ast.E_paren (e, _) -> Format.fprintf fmt "(%a)" pp_expr e
  | Ast.E_app (f, args, _) ->
    Format.fprintf fmt "%a(@[%a@])" pp_expr f (pp_comma_list pp_arg) args
  | Ast.E_let { pat; value; body; _ } ->
    Format.fprintf fmt "@[<v>let %a = %a in@,%a@]"
      pp_pattern pat pp_expr value pp_expr body
  | Ast.E_match { scrutinee; arms; _ } ->
    Format.fprintf fmt "@[<v>match %a {@,@[<v 2>  %a@]@,}@]"
      pp_expr scrutinee
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
         (fun fmt (p, e) -> Format.fprintf fmt "%a -> %a" pp_pattern p pp_expr e))
      arms
  | Ast.E_lambda { params; body; _ } ->
    Format.fprintf fmt "fn (@[%a@]) -> %a"
      (pp_comma_list pp_param) params pp_expr body
  | Ast.E_bin (op, l, r, _) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr l (bin_op_str op) pp_expr r
  | Ast.E_rel (op, l, r, _) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr l (rel_op_str op) pp_expr r
  | Ast.E_if { cond; then_; else_; _ } ->
    Format.fprintf fmt "(if %a then %a else %a)"
      pp_expr cond pp_expr then_ pp_expr else_
  | Ast.E_neg (e, _) ->
    Format.fprintf fmt "-%a" pp_expr e

and pp_arg fmt = function
  | Ast.A_pos e -> pp_expr fmt e
  | Ast.A_kw (n, e) -> Format.fprintf fmt "%s = %a" n pp_expr e

and pp_param fmt (p : Ast.param) =
  match p.annot with
  | None -> Format.fprintf fmt "%s" p.name
  | Some te -> Format.fprintf fmt "%s: %a" p.name pp_type_expr te

let pp_field_decl fmt (fd : Ast.field_decl) =
  Format.fprintf fmt "%s: %a" fd.name pp_type_expr fd.ty

let pp_ctor_decl fmt (cd : Ast.ctor_decl) =
  match cd.fields with
  | None -> Format.fprintf fmt "%s" cd.name
  | Some fs ->
    Format.fprintf fmt "%s {@[%a@]}" cd.name
      (pp_comma_list pp_field_decl) fs

let pp_top_decl fmt (d : Ast.top_decl) =
  match d with
  | Ast.D_type { name; ctors; span } ->
    Format.fprintf fmt "@[<v 2>D_type %s [%s]@,%a@]"
      name (pp_span span)
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut
         (fun fmt c -> Format.fprintf fmt "| %a" pp_ctor_decl c)) ctors
  | Ast.D_pile { name; params; card_ty; visibility; span } ->
    Format.fprintf fmt "@[<v 2>D_pile %s [%s]@,params: %a@,of: %a@,visibility: %a@]"
      name (pp_span span)
      (pp_comma_list pp_field_decl) params
      pp_type_expr card_ty
      pp_expr visibility
  | Ast.D_options { fields; span } ->
    Format.fprintf fmt "@[<v 2>D_options [%s]@,%a@]"
      (pp_span span)
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut
         (fun fmt (of_ : Ast.option_field) ->
            Format.fprintf fmt "%s: %a = %a"
              of_.name pp_type_expr of_.ty pp_expr of_.default))
      fields
  | Ast.D_fn { name; params; ret; body; span } ->
    Format.fprintf fmt "@[<v 2>D_fn %s [%s]@,params: (%a) -> %a@,body: %a@]"
      name (pp_span span)
      (pp_comma_list pp_param) params pp_type_expr ret pp_expr body
  | Ast.D_let { pat; body; span } ->
    Format.fprintf fmt "@[<v 2>D_let [%s]@,pat: %a@,body: %a@]"
      (pp_span span) pp_pattern pat pp_expr body

let dump_ast fmt (file : Ast.file) =
  Format.fprintf fmt "@[<v>File: %s@,%d declarations@,@,%a@]@."
    file.source_name (List.length file.decls)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,@,")
       pp_top_decl)
    file.decls

(* ---------------------------------------------------------------- *)
(* Typed AST                                                          *)
(* ---------------------------------------------------------------- *)

let rec pp_tpattern fmt (tp : Tc_ast.tpattern) =
  let pp_inner fmt = function
    | Tc_ast.TP_wild -> Format.fprintf fmt "_"
    | TP_var n -> Format.fprintf fmt "%s" n
    | TP_num n -> Format.fprintf fmt "%d" n
    | TP_ctor { name; fields; has_rest } ->
      if fields = [] then Format.fprintf fmt "%s" name
      else
        Format.fprintf fmt "%s {@[%a%s@]}" name
          (pp_comma_list (fun fmt (n, p) ->
             Format.fprintf fmt "%s: %a" n pp_tpattern p))
          fields
          (if has_rest then ", .." else "")
    | TP_tuple ps ->
      Format.fprintf fmt "(@[%a@])" (pp_comma_list pp_tpattern) ps
    | TP_list_exact ps ->
      Format.fprintf fmt "[@[%a@]]" (pp_comma_list pp_tpattern) ps
    | TP_list_cons { heads; rest } ->
      let rest_str = match rest with Some n -> ", .." ^ n | None -> ", .." in
      Format.fprintf fmt "[@[%a%s@]]" (pp_comma_list pp_tpattern) heads rest_str
  in
  Format.fprintf fmt "%a : %s" pp_inner tp.pat_node (Types.string_of_ty tp.pat_ty)

let rec pp_texpr fmt (te : Tc_ast.texpr) =
  let pp_inner fmt = function
    | Tc_ast.TE_num n -> Format.fprintf fmt "%d" n
    | TE_text s -> Format.fprintf fmt "%S" s
    | TE_var n -> Format.fprintf fmt "%s" n
    | TE_ctor (n, []) -> Format.fprintf fmt "%s" n
    | TE_ctor (n, fields) ->
      Format.fprintf fmt "%s {@[%a@]}" n
        (pp_comma_list (fun fmt (fn, fe) ->
           Format.fprintf fmt "%s: %a" fn pp_texpr fe)) fields
    | TE_record_update { ctor; spread; fields } ->
      Format.fprintf fmt "%s {..%a, @[%a@]}" ctor pp_texpr spread
        (pp_comma_list (fun fmt (fn, fe) ->
           Format.fprintf fmt "%s: %a" fn pp_texpr fe)) fields
    | TE_tuple es ->
      Format.fprintf fmt "(@[%a@])" (pp_comma_list pp_texpr) es
    | TE_list es ->
      Format.fprintf fmt "[@[%a@]]" (pp_comma_list pp_texpr) es
    | TE_app (f, args) ->
      Format.fprintf fmt "%a(@[%a@])" pp_texpr f (pp_comma_list pp_texpr) args
    | TE_let { pat; value; body } ->
      Format.fprintf fmt "@[<v>let %a = %a in@,%a@]"
        pp_tpattern pat pp_texpr value pp_texpr body
    | TE_match { scrutinee; arms } ->
      Format.fprintf fmt "@[<v>match %a {@,@[<v 2>  %a@]@,}@]"
        pp_texpr scrutinee
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
           (fun fmt (p, e) ->
              Format.fprintf fmt "%a -> %a" pp_tpattern p pp_texpr e)) arms
    | TE_lambda { params; body } ->
      Format.fprintf fmt "fn (@[%a@]) -> %a"
        (pp_comma_list (fun fmt (n, ty) ->
           Format.fprintf fmt "%s: %s" n (Types.string_of_ty ty))) params
        pp_texpr body
    | TE_bin (op, l, r) ->
      Format.fprintf fmt "(%a %s %a)" pp_texpr l (bin_op_str op) pp_texpr r
    | TE_rel (op, l, r) ->
      Format.fprintf fmt "(%a %s %a)" pp_texpr l (rel_op_str op) pp_texpr r
    | TE_if { cond; then_; else_ } ->
      Format.fprintf fmt "(if %a then %a else %a)"
        pp_texpr cond pp_texpr then_ pp_texpr else_
    | TE_neg e -> Format.fprintf fmt "-%a" pp_texpr e
  in
  Format.fprintf fmt "(%a : %s)" pp_inner te.node (Types.string_of_ty te.ty)

let pp_type_info fmt = function
  | Types.TI_builtin_opaque -> Format.fprintf fmt "<opaque>"
  | Types.TI_adt { ctors; is_record } ->
    Format.fprintf fmt "%s [%a]"
      (if is_record then "record" else "sum")
      (pp_comma_list (fun fmt (c : Types.ctor_info) ->
         if c.fields = [] then Format.fprintf fmt "%s" c.ctor_name
         else
           Format.fprintf fmt "%s {%a}" c.ctor_name
             (pp_comma_list (fun fmt (n, t) ->
                Format.fprintf fmt "%s: %s" n (Types.string_of_ty t)))
             c.fields)) ctors

let dump_tast fmt (tf : Typecheck.tfile) =
  Format.fprintf fmt "@[<v>Typed file: %s@,@," (Typecheck.source_name tf);
  Format.fprintf fmt "@[<v 2>Type decls:@,%a@]@,@,"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (n, info) ->
          Format.fprintf fmt "type %s = %a" n pp_type_info info))
    (Typecheck.type_decls tf);
  Format.fprintf fmt "@[<v 2>Pile decls:@,%a@]@,@,"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (name, params, card_ty, vis) ->
          Format.fprintf fmt
            "@[<v 2>pile %s(%a) of %s@,visibility = %a@]"
            name
            (pp_comma_list (fun fmt (n, t) ->
               Format.fprintf fmt "%s: %s" n (Types.string_of_ty t))) params
            (Types.string_of_ty card_ty) pp_texpr vis))
    (Typecheck.pile_decls tf);
  Format.fprintf fmt "@[<v 2>Options decl (%d fields):@,%a@]@,@,"
    (List.length (Typecheck.options_decl tf))
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (n, t, def) ->
          Format.fprintf fmt "%s: %s = %a" n (Types.string_of_ty t) pp_texpr def))
    (Typecheck.options_decl tf);
  Format.fprintf fmt "@[<v 2>Top-level fns:@,%a@]@,@,"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (n, e) ->
          Format.fprintf fmt "@[<v 2>fn %s :: %s@,= %a@]"
            n (Types.string_of_ty e.Tc_ast.ty) pp_texpr e))
    (Typecheck.top_fns tf);
  Format.fprintf fmt "@[<v 2>Top-level lets:@,%a@]@]@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (p, e) ->
          Format.fprintf fmt "@[<v 2>let %a@,= %a@]" pp_tpattern p pp_texpr e))
    (Typecheck.top_lets tf)

(* ---------------------------------------------------------------- *)
(* Link summary                                                       *)
(* ---------------------------------------------------------------- *)

let dump_link fmt (l : Link.t) =
  Format.fprintf fmt "@[<v>Linked ruleset: %s@,@,"
    (Typecheck.source_name l.tfile);
  Format.fprintf fmt "Required types: %s, %s, %s, %s, %s@,@,"
    l.card_type l.action_type l.outcome_type l.config_type l.player_dict_type;
  Format.fprintf fmt "@[<v 2>Required functions:@,%a@]@,@,"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (label, e) ->
          Format.fprintf fmt "%-18s :: %s"
            label (Types.string_of_ty e.Tc_ast.ty)))
    [ "setup",           l.setup;
      "validate",        l.validate;
      "apply",           l.apply;
      "terminal",        l.terminal;
      "action_to_text",  l.action_to_text;
      "text_to_action",  l.text_to_action;
      "view_to_text",    l.view_to_text;
      "outcome_to_text", l.outcome_to_text ];
  Format.fprintf fmt "@[<v 2>Piles (%d):@,%a@]@,@,"
    (List.length l.pile_decls)
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (p : Link.pile_info) ->
          Format.fprintf fmt "%s(%a) of %s"
            p.name
            (pp_comma_list (fun fmt (n, t) ->
               Format.fprintf fmt "%s: %s" n (Types.string_of_ty t))) p.key_params
            (Types.string_of_ty p.card_ty)))
    l.pile_decls;
  Format.fprintf fmt "@[<v 2>Options schema (%d fields):@,%a@]@]@."
    (List.length l.options_schema)
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut
       (fun fmt (f : Link.options_field) ->
          Format.fprintf fmt "%s: %s" f.name (Types.string_of_ty f.ty)))
    l.options_schema

(* ---------------------------------------------------------------- *)
(* Runtime values (uniform named ADT form per design decision).      *)
(* ---------------------------------------------------------------- *)

let rec pp_value fmt (v : Value.t) =
  match v with
  | V_num n -> Format.fprintf fmt "%d" n
  | V_text s -> Format.fprintf fmt "%S" s
  | V_player p -> Format.fprintf fmt "%S" p
  | V_unit -> Format.fprintf fmt "Unit"
  | V_ctor { name; fields = [] } -> Format.fprintf fmt "%s" name
  | V_ctor { name; fields } ->
    Format.fprintf fmt "%s(@[%a@])" name
      (pp_comma_list (fun fmt (fn, fv) ->
         Format.fprintf fmt "%s=%a" fn pp_value fv))
      fields
  | V_tuple vs ->
    Format.fprintf fmt "(@[%a@])" (pp_comma_list pp_value) vs
  | V_list vs ->
    Format.fprintf fmt "[@[%a@]]" (pp_comma_list pp_value) vs
  | V_pile_ref { name; keys = [] } ->
    Format.fprintf fmt "<pile %s>" name
  | V_pile_ref { name; keys } ->
    Format.fprintf fmt "<pile %s(@[%a@])>" name (pp_comma_list pp_value) keys
  | V_fn _ -> Format.fprintf fmt "<fn>"
  | V_builtin n -> Format.fprintf fmt "<builtin %s>" n
  | V_pile_ctor { name; arity } ->
    Format.fprintf fmt "<pile_ctor %s/%d>" name arity
  | V_partial { arity; _ } ->
    Format.fprintf fmt "<partial/%d>" arity
  | V_state _ -> Format.fprintf fmt "<state>"
  | V_view _ -> Format.fprintf fmt "<view>"
  | V_rng -> Format.fprintf fmt "<rng>"

(* ---------------------------------------------------------------- *)
(* Errors                                                             *)
(* ---------------------------------------------------------------- *)

let dump_errors fmt (errs : Load_error.t list) =
  List.iter (fun e ->
    Format.fprintf fmt "%s@." (Load_error.to_string e)) errs
