(** Typed-AST types produced by the checker.

    Extracted from [Typecheck] so the internal expression checker
    ([Tc_expr]) can produce these without dragging in the public
    [Typecheck] entry module — that would create a dependency cycle.

    [Typecheck.mli] re-exports [texpr] and [tpattern] as type-equality
    aliases so external code can pattern-match on the concrete shape. *)

type texpr = {
  node : texpr_node;
  ty : Types.ty;
  span : Ast.span;
}

and texpr_node =
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
  | TE_rel of Ast.rel_op * texpr * texpr
  | TE_if of { cond : texpr; then_ : texpr; else_ : texpr }
  | TE_neg of texpr

and tpattern = {
  pat_node : tpattern_node;
  pat_ty : Types.ty;
  pat_span : Ast.span;
}

and tpattern_node =
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
