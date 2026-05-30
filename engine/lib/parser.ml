open Core

let parse ~source_name tokens =
  let toks = Array.of_list tokens in
  let pos = ref 0 in
  let exception Parse_err of string * Load_error.span in

  let at n : Token.positioned = toks.(n) in
  let peek () = (at !pos).token in
  let peek_span () = (at !pos).span in
  let peek_at n =
    if !pos + n < Array.length toks then (at (!pos + n)).token
    else Token.EOF
  in
  let advance () = let t = at !pos in incr pos; t in
  let err msg = raise (Parse_err (msg, peek_span ())) in

  let span_union (a : Load_error.span) (b : Load_error.span) : Load_error.span =
    { file = a.file;
      start_line = a.start_line; start_col = a.start_col;
      end_line = b.end_line; end_col = b.end_col }
  in

  let expect tok =
    if Poly.(peek () = tok) then ignore (advance ())
    else err (Printf.sprintf "expected %s, got %s"
      (Token.to_string tok) (Token.to_string (peek ())))
  in
  let eat tok =
    if Poly.(peek () = tok) then (ignore (advance ()); true) else false
  in
  let expect_value_ident () =
    match peek () with
    | Token.VALUE_IDENT s ->
      let sp = peek_span () in ignore (advance ()); (s, sp)
    | t -> err (Printf.sprintf "expected identifier, got %s" (Token.to_string t))
  in
  let expect_type_ident () =
    match peek () with
    | Token.TYPE_IDENT s ->
      let sp = peek_span () in ignore (advance ()); (s, sp)
    | t -> err (Printf.sprintf "expected type name, got %s" (Token.to_string t))
  in

  let rec parse_type_expr () =
    let start = peek_span () in
    match peek () with
    | Token.TYPE_IDENT s ->
      ignore (advance ());
      if Poly.(peek () = Token.LT) then begin
        ignore (advance ());
        let first = parse_type_expr () in
        let rec loop acc =
          if eat Token.COMMA then loop (parse_type_expr () :: acc)
          else List.rev acc
        in
        let rest = loop [] in
        let end_sp = peek_span () in
        expect Token.GT;
        Ast.TE_app (s, first :: rest, span_union start end_sp)
      end else
        Ast.TE_app (s, [], start)
    | Token.VALUE_IDENT s ->
      ignore (advance ());
      Ast.TE_var (s, start)
    | Token.LPAREN ->
      ignore (advance ());
      if Poly.(peek () = Token.RPAREN) then begin
        ignore (advance ());
        expect Token.ARROW;
        let ret = parse_type_expr () in
        Ast.TE_fn ([], ret, span_union start (Ast.span_of_type_expr ret))
      end else begin
        let first = parse_type_expr () in
        if eat Token.COMMA then begin
          let rec loop acc =
            let t = parse_type_expr () in
            let acc = t :: acc in
            if eat Token.COMMA then loop acc
            else List.rev acc
          in
          let rest = loop [] in
          let end_sp = peek_span () in
          expect Token.RPAREN;
          let elems = first :: rest in
          if eat Token.ARROW then
            let ret = parse_type_expr () in
            Ast.TE_fn (elems, ret, span_union start (Ast.span_of_type_expr ret))
          else
            Ast.TE_tuple (elems, span_union start end_sp)
        end else begin
          expect Token.RPAREN;
          if eat Token.ARROW then
            let ret = parse_type_expr () in
            Ast.TE_fn ([first], ret, span_union start (Ast.span_of_type_expr ret))
          else
            err "parenthesized type must be a tuple (2+ elements) or function type '(...) -> T'"
        end
      end
    | t -> err (Printf.sprintf "expected type, got %s" (Token.to_string t))

  and parse_pattern () =
    let start = peek_span () in
    match peek () with
    | Token.VALUE_IDENT s ->
      ignore (advance ());
      if String.equal s "_" then Ast.P_wild start
      else Ast.P_var (s, start)
    | Token.NUM_LIT n ->
      ignore (advance ());
      Ast.P_num (n, start)
    | Token.TYPE_IDENT s ->
      ignore (advance ());
      if Poly.(peek () = Token.LBRACE) then begin
        ignore (advance ());
        let (fields, has_rest, end_sp) = parse_pattern_body () in
        Ast.P_ctor { name = s; fields = Some fields; has_rest;
                     span = span_union start end_sp }
      end else if Poly.(peek () = Token.LPAREN) then begin
        ignore (advance ());
        if Poly.(peek () = Token.RPAREN) then
          err "constructor pattern must have at least one argument; \
               use the nullary form (just the constructor name) or 'Ctor {}'";
        let acc = ref [] in
        let rec loop () =
          acc := parse_pattern () :: !acc;
          if eat Token.COMMA then
            if Poly.(peek () = Token.RPAREN) then ()
            else loop ()
        in
        loop ();
        let args = List.rev !acc in
        let end_sp = peek_span () in
        expect Token.RPAREN;
        Ast.P_ctor_pos { name = s; args; span = span_union start end_sp }
      end else
        Ast.P_ctor { name = s; fields = None; has_rest = false; span = start }
    | Token.LPAREN ->
      ignore (advance ());
      let first = parse_pattern () in
      expect Token.COMMA;
      let rec loop acc =
        let p = parse_pattern () in
        let acc = p :: acc in
        if eat Token.COMMA then
          if Poly.(peek () = Token.RPAREN) then List.rev acc
          else loop acc
        else List.rev acc
      in
      let rest = loop [] in
      let end_sp = peek_span () in
      expect Token.RPAREN;
      Ast.P_tuple (first :: rest, span_union start end_sp)
    | Token.LBRACK ->
      ignore (advance ());
      if Poly.(peek () = Token.RBRACK) then begin
        let end_sp = peek_span () in
        expect Token.RBRACK;
        Ast.P_list_exact ([], span_union start end_sp)
      end else
        parse_list_pattern_rest start
    | t -> err (Printf.sprintf "expected pattern, got %s" (Token.to_string t))

  and parse_list_pattern_rest start =
    let rec loop acc =
      let p = parse_pattern () in
      let acc = p :: acc in
      if eat Token.COMMA then
        if Poly.(peek () = Token.DOTDOT) then begin
          ignore (advance ());
          let rest = match peek () with
            | Token.VALUE_IDENT s -> ignore (advance ()); Some s
            | _ -> None
          in
          let end_sp = peek_span () in
          expect Token.RBRACK;
          Ast.P_list_cons { heads = List.rev acc; rest;
                            span = span_union start end_sp }
        end else if Poly.(peek () = Token.RBRACK) then begin
          let end_sp = peek_span () in
          expect Token.RBRACK;
          Ast.P_list_exact (List.rev acc, span_union start end_sp)
        end else loop acc
      else begin
        let end_sp = peek_span () in
        expect Token.RBRACK;
        Ast.P_list_exact (List.rev acc, span_union start end_sp)
      end
    in
    loop []

  and parse_pattern_body () =
    (* Already consumed LBRACE. Consumes RBRACE. *)
    if Poly.(peek () = Token.RBRACE) then begin
      let end_sp = peek_span () in
      expect Token.RBRACE;
      ([], false, end_sp)
    end else begin
      let fields = ref [] in
      let has_rest = ref false in
      let rec loop () =
        if Poly.(peek () = Token.DOTDOT) then begin
          ignore (advance ());
          has_rest := true;
          ignore (eat Token.COMMA)
        end else begin
          let fp = parse_field_pat () in
          fields := fp :: !fields;
          if eat Token.COMMA then
            if Poly.(peek () = Token.RBRACE) then ()
            else loop ()
        end
      in
      loop ();
      let end_sp = peek_span () in
      expect Token.RBRACE;
      (List.rev !fields, !has_rest, end_sp)
    end

  and parse_field_pat () =
    let (name, start) = expect_value_ident () in
    let sub =
      if eat Token.COLON then Some (parse_pattern ())
      else None
    in
    let end_sp = match sub with
      | Some p -> Ast.span_of_pattern p
      | None -> start
    in
    { Ast.field = name; sub; span = span_union start end_sp }

  and parse_expr () = parse_spine ()

  and parse_spine () =
    match peek () with
    | Token.KW_LET -> parse_let_expr ()
    | Token.KW_MATCH -> parse_match_expr ()
    | Token.KW_FN -> parse_lambda ()
    | _ -> parse_add ()

  and parse_let_expr () =
    let start = peek_span () in
    expect Token.KW_LET;
    let pat = parse_pattern () in
    expect Token.EQ;
    let value = parse_spine () in
    expect Token.KW_IN;
    let body = parse_spine () in
    Ast.E_let { pat; value; body;
                span = span_union start (Ast.span_of_expr body) }

  and parse_match_expr () =
    let start = peek_span () in
    expect Token.KW_MATCH;
    let scrutinee = parse_expr () in
    expect Token.LBRACE;
    let arms = parse_match_arms () in
    let end_sp = peek_span () in
    expect Token.RBRACE;
    Ast.E_match { scrutinee; arms; span = span_union start end_sp }

  and parse_match_arms () =
    let rec loop acc =
      let arm = parse_match_arm () in
      let acc = arm :: acc in
      if eat Token.SEMI then
        if Poly.(peek () = Token.RBRACE) then List.rev acc
        else loop acc
      else List.rev acc
    in
    loop []

  and parse_match_arm () =
    let pat = parse_pattern () in
    expect Token.ARROW;
    let body = parse_spine () in
    (pat, body)

  and parse_lambda () =
    let start = peek_span () in
    expect Token.KW_FN;
    expect Token.LPAREN;
    let params = parse_params_until Token.RPAREN in
    expect Token.RPAREN;
    expect Token.ARROW;
    let body = parse_spine () in
    Ast.E_lambda { params; body;
                   span = span_union start (Ast.span_of_expr body) }

  and parse_params_until stop =
    if Poly.(peek () = stop) then []
    else
      let rec loop acc =
        let p = parse_param () in
        let acc = p :: acc in
        if eat Token.COMMA then
          if Poly.(peek () = stop) then List.rev acc
          else loop acc
        else List.rev acc
      in
      loop []

  and parse_param () =
    let (name, start) = expect_value_ident () in
    let annot =
      if eat Token.COLON then Some (parse_type_expr ())
      else None
    in
    let end_sp = match annot with
      | Some t -> Ast.span_of_type_expr t
      | None -> start
    in
    { Ast.name; annot; span = span_union start end_sp }

  and parse_add () =
    let left = ref (parse_mul ()) in
    let go = ref true in
    while !go do
      match peek () with
      | Token.PLUS ->
        ignore (advance ());
        let r = parse_mul () in
        let sp = span_union (Ast.span_of_expr !left) (Ast.span_of_expr r) in
        left := Ast.E_bin (Ast.Add, !left, r, sp)
      | Token.MINUS ->
        ignore (advance ());
        let r = parse_mul () in
        let sp = span_union (Ast.span_of_expr !left) (Ast.span_of_expr r) in
        left := Ast.E_bin (Ast.Sub, !left, r, sp)
      | _ -> go := false
    done;
    !left

  and parse_mul () =
    let left = ref (parse_unary ()) in
    let go = ref true in
    while !go do
      let op = match peek () with
        | Token.STAR -> Some Ast.Mul
        | Token.SLASH -> Some Ast.Div
        | Token.KW_MOD -> Some Ast.Mod
        | _ -> None
      in
      match op with
      | Some o ->
        ignore (advance ());
        let r = parse_unary () in
        let sp = span_union (Ast.span_of_expr !left) (Ast.span_of_expr r) in
        left := Ast.E_bin (o, !left, r, sp)
      | None -> go := false
    done;
    !left

  and parse_unary () =
    if Poly.(peek () = Token.MINUS) then begin
      let start = peek_span () in
      ignore (advance ());
      let inner = parse_unary () in
      Ast.E_neg (inner, span_union start (Ast.span_of_expr inner))
    end else parse_app ()

  and parse_app () =
    let callee = ref (parse_atom ()) in
    while Poly.(peek () = Token.LPAREN) do
      ignore (advance ());
      let args = parse_args_until Token.RPAREN in
      let end_sp = peek_span () in
      expect Token.RPAREN;
      callee := Ast.E_app (!callee, args,
                           span_union (Ast.span_of_expr !callee) end_sp)
    done;
    !callee

  and parse_args_until stop =
    if Poly.(peek () = stop) then []
    else
      let rec loop acc =
        let a = parse_arg () in
        let acc = a :: acc in
        if eat Token.COMMA then
          if Poly.(peek () = stop) then List.rev acc
          else loop acc
        else List.rev acc
      in
      loop []

  and parse_arg () =
    match peek () with
    | Token.VALUE_IDENT s when Poly.(peek_at 1 = Token.EQ) ->
      ignore (advance ());
      expect Token.EQ;
      let e = parse_expr () in
      Ast.A_kw (s, e)
    | _ -> Ast.A_pos (parse_expr ())

  and parse_atom () =
    let start = peek_span () in
    match peek () with
    | Token.NUM_LIT n -> ignore (advance ()); Ast.E_num (n, start)
    | Token.TEXT_LIT s -> ignore (advance ()); Ast.E_text (s, start)
    | Token.VALUE_IDENT "_" ->
      err "wildcard '_' is only valid in patterns, not in expressions"
    | Token.VALUE_IDENT s -> ignore (advance ()); Ast.E_var (s, start)
    | Token.TYPE_IDENT s ->
      ignore (advance ());
      if Poly.(peek () = Token.LBRACE) then begin
        ignore (advance ());
        let body = parse_record_body () in
        let end_sp = peek_span () in
        expect Token.RBRACE;
        Ast.E_record { ctor = s; body; span = span_union start end_sp }
      end else
        Ast.E_ctor (s, start)
    | Token.LPAREN ->
      ignore (advance ());
      let first = parse_expr () in
      if eat Token.COMMA then begin
        let rec loop acc =
          let e = parse_expr () in
          let acc = e :: acc in
          if eat Token.COMMA then
            if Poly.(peek () = Token.RPAREN) then List.rev acc
            else loop acc
          else List.rev acc
        in
        let rest = loop [] in
        let end_sp = peek_span () in
        expect Token.RPAREN;
        Ast.E_tuple (first :: rest, span_union start end_sp)
      end else begin
        let end_sp = peek_span () in
        expect Token.RPAREN;
        Ast.E_paren (first, span_union start end_sp)
      end
    | Token.LBRACK ->
      ignore (advance ());
      if Poly.(peek () = Token.RBRACK) then begin
        let end_sp = peek_span () in
        expect Token.RBRACK;
        Ast.E_list ([], span_union start end_sp)
      end else begin
        let first = parse_expr () in
        let rec loop acc =
          if eat Token.COMMA then
            if Poly.(peek () = Token.RBRACK) then List.rev acc
            else loop (parse_expr () :: acc)
          else List.rev acc
        in
        let rest = loop [] in
        let end_sp = peek_span () in
        expect Token.RBRACK;
        Ast.E_list (first :: rest, span_union start end_sp)
      end
    | Token.PIPE_FORWARD ->
      err "'|>' is reserved for future use"
    | t -> err (Printf.sprintf "unexpected %s" (Token.to_string t))

  and parse_record_body () =
    let parse_field_inits () =
      let acc = ref [] in
      let rec loop () =
        let (name, name_sp) = expect_value_ident () in
        let v =
          if eat Token.COLON then parse_expr ()
          else Ast.E_var (name, name_sp)
        in
        acc := (name, v) :: !acc;
        if eat Token.COMMA then
          if Poly.(peek () = Token.RBRACE) then ()
          else loop ()
      in
      loop ();
      List.rev !acc
    in
    if Poly.(peek () = Token.DOTDOT) then begin
      ignore (advance ());
      let sp = parse_expr () in
      (* Spread requires at least one override field. *)
      if not (eat Token.COMMA) then
        err "record update with spread requires at least one override field; \
             write the spread expression directly if no fields change";
      if Poly.(peek () = Token.RBRACE) then
        err "record update with spread requires at least one override field; \
             write the spread expression directly if no fields change";
      { Ast.spread = Some sp; fields = parse_field_inits () }
    end else if Poly.(peek () = Token.RBRACE) then
      { Ast.spread = None; fields = [] }
    else
      { Ast.spread = None; fields = parse_field_inits () }

  and parse_constructor () =
    let (name, start) = expect_type_ident () in
    if Poly.(peek () = Token.LBRACE) then begin
      ignore (advance ());
      let fields =
        if Poly.(peek () = Token.RBRACE) then []
        else begin
          let acc = ref [] in
          let rec loop () =
            acc := parse_field_decl () :: !acc;
            if eat Token.COMMA then
              if Poly.(peek () = Token.RBRACE) then ()
              else loop ()
          in
          loop ();
          List.rev !acc
        end
      in
      let end_sp = peek_span () in
      expect Token.RBRACE;
      { Ast.name; fields = Some fields; span = span_union start end_sp }
    end else
      { Ast.name; fields = None; span = start }

  and parse_field_decl () =
    let (name, start) = expect_value_ident () in
    expect Token.COLON;
    let ty = parse_type_expr () in
    { Ast.name; ty; span = span_union start (Ast.span_of_type_expr ty) }

  and parse_option_field () =
    let (name, start) = expect_value_ident () in
    expect Token.COLON;
    let ty = parse_type_expr () in
    expect Token.EQ;
    let default = parse_expr () in
    { Ast.name; ty; default;
      span = span_union start (Ast.span_of_expr default) }

  and parse_type_decl () =
    let start = peek_span () in
    expect Token.KW_TYPE;
    let (name, _) = expect_type_ident () in
    expect Token.EQ;
    ignore (eat Token.PIPE);  (* optional leading '|' for vertical layout *)
    let ctors = ref [parse_constructor ()] in
    let end_sp = ref (List.hd_exn !ctors).Ast.span in
    let go = ref true in
    while !go do
      if eat Token.PIPE then begin
        let c = parse_constructor () in
        ctors := c :: !ctors;
        end_sp := c.Ast.span
      end else go := false
    done;
    Ast.D_type { name; ctors = List.rev !ctors;
                 span = span_union start !end_sp }

  and parse_pile_decl () =
    let start = peek_span () in
    expect Token.KW_PILE;
    let (name, _) = expect_type_ident () in
    let params =
      if eat Token.LPAREN then begin
        let fs =
          if Poly.(peek () = Token.RPAREN) then []
          else begin
            let acc = ref [] in
            let rec loop () =
              acc := parse_field_decl () :: !acc;
              if eat Token.COMMA then
                if Poly.(peek () = Token.RPAREN) then ()
                else loop ()
            in
            loop ();
            List.rev !acc
          end
        in
        expect Token.RPAREN;
        fs
      end else []
    in
    expect Token.KW_OF;
    let card_ty = parse_type_expr () in
    expect Token.KW_VISIBILITY;
    expect Token.EQ;
    let visibility = parse_expr () in
    Ast.D_pile { name; params; card_ty; visibility;
                 span = span_union start (Ast.span_of_expr visibility) }

  and parse_options_decl () =
    let start = peek_span () in
    (* "options" is a soft keyword — lexed as VALUE_IDENT, recognized here. *)
    (match peek () with
     | Token.VALUE_IDENT "options" -> ignore (advance ())
     | t -> err (Printf.sprintf "expected 'options', got %s" (Token.to_string t)));
    expect Token.LBRACE;
    let fields =
      if Poly.(peek () = Token.RBRACE) then []
      else begin
        let acc = ref [] in
        let rec loop () =
          acc := parse_option_field () :: !acc;
          if eat Token.COMMA then
            if Poly.(peek () = Token.RBRACE) then ()
            else loop ()
        in
        loop ();
        List.rev !acc
      end
    in
    let end_sp = peek_span () in
    expect Token.RBRACE;
    Ast.D_options { fields; span = span_union start end_sp }

  and parse_fn_decl () =
    let start = peek_span () in
    expect Token.KW_FN;
    let (name, _) = expect_value_ident () in
    expect Token.LPAREN;
    let params = parse_params_until Token.RPAREN in
    expect Token.RPAREN;
    expect Token.ARROW;
    let ret = parse_type_expr () in
    expect Token.EQ;
    let body = parse_expr () in
    Ast.D_fn { name; params; ret; body;
               span = span_union start (Ast.span_of_expr body) }

  and parse_let_decl () =
    let start = peek_span () in
    expect Token.KW_LET;
    let pat = parse_pattern () in
    expect Token.EQ;
    let body = parse_expr () in
    Ast.D_let { pat; body;
                span = span_union start (Ast.span_of_expr body) }

  and parse_top_decl () =
    match peek () with
    | Token.KW_TYPE -> parse_type_decl ()
    | Token.KW_PILE -> parse_pile_decl ()
    | Token.KW_FN -> parse_fn_decl ()
    | Token.KW_LET -> parse_let_decl ()
    | Token.VALUE_IDENT "options"
      when Poly.(peek_at 1 = Token.LBRACE) -> parse_options_decl ()
    | t -> err (Printf.sprintf "expected top-level declaration, got %s"
                  (Token.to_string t))
  in

  try
    let decls = ref [] in
    while Poly.(peek () <> Token.EOF) do
      decls := parse_top_decl () :: !decls
    done;
    Ok { Ast.source_name; decls = List.rev !decls }
  with Parse_err (msg, sp) ->
    Error (Load_error.make Load_error.Parse sp msg)
