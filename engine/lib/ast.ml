type span = Load_error.span

type type_expr =
  | TE_app of string * type_expr list * span
  | TE_tuple of type_expr list * span
  | TE_fn of type_expr list * type_expr * span
  | TE_var of string * span

type pattern =
  | P_wild of span
  | P_var of string * span
  | P_num of int * span
  | P_ctor of {
      name : string;
      fields : field_pat list option;
      has_rest : bool;
      span : span;
    }
  | P_ctor_pos of {
      name : string;
      args : pattern list;
      span : span;
    }
  | P_tuple of pattern list * span
  | P_list_exact of pattern list * span
  | P_list_cons of {
      heads : pattern list;
      rest : string option;
      span : span;
    }

and field_pat = {
  field : string;
  sub : pattern option;
  span : span;
}

type expr =
  | E_num of int * span
  | E_text of string * span
  | E_var of string * span
  | E_ctor of string * span
  | E_record of {
      ctor : string;
      body : record_body;
      span : span;
    }
  | E_tuple of expr list * span
  | E_list of expr list * span
  | E_paren of expr * span
  | E_app of expr * arg list * span
  | E_let of {
      pat : pattern;
      value : expr;
      body : expr;
      span : span;
    }
  | E_match of {
      scrutinee : expr;
      arms : (pattern * expr) list;
      span : span;
    }
  | E_lambda of {
      params : param list;
      body : expr;
      span : span;
    }
  | E_bin of bin_op * expr * expr * span
  | E_rel of rel_op * expr * expr * span
  | E_if of {
      cond : expr;
      then_ : expr;
      else_ : expr;
      span : span;
    }
  | E_neg of expr * span

and record_body = {
  spread : expr option;
  fields : (string * expr) list;
}

and arg =
  | A_pos of expr
  | A_kw of string * expr

and param = {
  name : string;
  annot : type_expr option;
  span : span;
}

and bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

and rel_op =
  | RLt
  | RLte
  | RGt
  | RGte
  | REq
  | RNeq

type field_decl = {
  name : string;
  ty : type_expr;
  span : span;
}

type ctor_decl = {
  name : string;
  fields : field_decl list option;
  span : span;
}

type option_field = {
  name : string;
  ty : type_expr;
  default : expr;
  span : span;
}

type top_decl =
  | D_type of {
      name : string;
      ctors : ctor_decl list;
      span : span;
    }
  | D_pile of {
      name : string;
      params : field_decl list;
      card_ty : type_expr;
      visibility : expr;
      span : span;
    }
  | D_options of {
      fields : option_field list;
      span : span;
    }
  | D_fn of {
      name : string;
      params : param list;
      ret : type_expr;
      body : expr;
      span : span;
    }
  | D_let of {
      pat : pattern;
      body : expr;
      span : span;
    }

type file = {
  source_name : string;
  decls : top_decl list;
}

let span_of_expr = function
  | E_num (_, s) | E_text (_, s) | E_var (_, s) | E_ctor (_, s)
  | E_tuple (_, s) | E_list (_, s) | E_paren (_, s) | E_neg (_, s) -> s
  | E_record { span; _ } | E_let { span; _ } | E_match { span; _ }
  | E_lambda { span; _ } | E_if { span; _ } -> span
  | E_app (_, _, s) -> s
  | E_bin (_, _, _, s) | E_rel (_, _, _, s) -> s

let span_of_pattern = function
  | P_wild s | P_var (_, s) | P_num (_, s) -> s
  | P_ctor { span; _ } | P_ctor_pos { span; _ } -> span
  | P_tuple (_, s) | P_list_exact (_, s) -> s
  | P_list_cons { span; _ } -> span

let span_of_type_expr = function
  | TE_app (_, _, s) | TE_tuple (_, s) | TE_var (_, s) -> s
  | TE_fn (_, _, s) -> s

let span_of_top_decl = function
  | D_type { span; _ } | D_pile { span; _ } | D_options { span; _ }
  | D_fn { span; _ } | D_let { span; _ } -> span
