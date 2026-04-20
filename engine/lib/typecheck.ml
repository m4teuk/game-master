type texpr = Tc_ast.texpr = {
  node : texpr_node;
  ty : Types.ty;
  span : Ast.span;
}

and texpr_node = Tc_ast.texpr_node =
  | TE_num of int
  | TE_text of string
  | TE_var of string
  | TE_ctor of string * (string * texpr) list
  | TE_record_update of {
      ctor : string;
      spread : texpr;
      fields : (string * texpr) list;
    }
  | TE_tuple of texpr list
  | TE_list of texpr list
  | TE_app of texpr * texpr list
  | TE_let of { pat : tpattern; value : texpr; body : texpr }
  | TE_match of { scrutinee : texpr; arms : (tpattern * texpr) list }
  | TE_lambda of {
      params : (string * Types.ty) list;
      body : texpr;
    }
  | TE_bin of Ast.bin_op * texpr * texpr
  | TE_neg of texpr

and tpattern = Tc_ast.tpattern = {
  pat_node : tpattern_node;
  pat_ty : Types.ty;
  pat_span : Ast.span;
}

and tpattern_node = Tc_ast.tpattern_node =
  | TP_wild
  | TP_var of string
  | TP_num of int
  | TP_ctor of {
      name : string;
      fields : (string * tpattern) list;
      has_rest : bool;
    }
  | TP_tuple of tpattern list
  | TP_list_exact of tpattern list
  | TP_list_cons of {
      heads : tpattern list;
      rest : string option;
    }

type tfile = {
  source_name : string;
  type_decls : (string * Types.type_info) list;
  pile_decls : (string * (string * Types.ty) list * Types.ty * texpr) list;
  options_decl : (string * Types.ty * texpr) list;
  top_fns : (string * texpr) list;
  top_lets : (tpattern * texpr) list;
}

let expr_type (e : texpr) = e.ty

let initial_env () =
  Types.empty
  |> Builtins.seed_types
  |> Builtins.seed_values

let check (file : Ast.file) =
  let errs = Tc_errors.create () in
  let env0 = initial_env () in
  let decls = Tc_pass_decls.run env0 errs file in
  let env, exprs = Tc_pass_exprs.run decls.env errs decls in
  match Tc_errors.collected errs with
  | [] ->
    Ok ({
      source_name = file.source_name;
      type_decls = decls.type_decls;
      pile_decls = exprs.pile_decls;
      options_decl = exprs.options_decl;
      top_fns = exprs.top_fns;
      top_lets = exprs.top_lets;
    }, env)
  | es -> Error es

let top_fns      (f : tfile) = f.top_fns
let top_lets     (f : tfile) = f.top_lets
let type_decls   (f : tfile) = f.type_decls
let pile_decls   (f : tfile) = f.pile_decls
let options_decl (f : tfile) = f.options_decl
let source_name  (f : tfile) = f.source_name
