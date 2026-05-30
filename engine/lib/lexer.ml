open Core

let tokenize ~file src =
  let pos = ref 0 and line = ref 1 and col = ref 1 in
  let tokens = ref [] in
  let exception Lex_err of string * Load_error.span in

  let mark () = (!line, !col) in

  let span_from (l0, c0) =
    { Load_error.file; start_line = l0; start_col = c0;
      end_line = !line; end_col = !col }
  in

  let emit tok span = tokens := { Token.token = tok; span } :: !tokens in
  
  let incr_cur () = incr col; incr pos in

  let scan_token () =
    let start = mark () in
    let emit_incr tok = emit tok (span_from start); incr_cur () in
    let next_eq (c: char) = !pos + 1 < String.length src && Char.equal src.[!pos + 1] c in

    let read_word () =
      let start_idx = !pos in
      while !pos + 1 < String.length src &&
        (Char.is_alphanum src.[!pos + 1] || Char.equal src.[!pos + 1] '_') do
        incr_cur ()
      done;
      String.sub src ~pos:start_idx ~len:(!pos - start_idx + 1)
    in

    match src.[!pos] with
    | '(' -> emit_incr Token.LPAREN
    | ')' -> emit_incr Token.RPAREN
    | '{' -> emit_incr Token.LBRACE
    | '}' -> emit_incr Token.RBRACE
    | '[' -> emit_incr Token.LBRACK
    | ']' -> emit_incr Token.RBRACK
    | ',' -> emit_incr Token.COMMA
    | ':' -> emit_incr Token.COLON
    | ';' -> emit_incr Token.SEMI
    | '=' -> emit_incr Token.EQ

    | '-' -> if next_eq '>' then (
        incr_cur ();
        emit_incr Token.ARROW
      ) else if next_eq '-' then (
        (* comment, skip to \n *)
        incr_cur ();
        while !pos < String.length src && Char.(<>) src.[!pos] '\n' do
          incr_cur ()
        done
      ) else
        emit_incr Token.MINUS

    | '|' -> if next_eq '>' then (
        incr_cur ();
        emit_incr Token.PIPE_FORWARD
      ) else
        emit_incr Token.PIPE

    | '.' -> if next_eq '.' then (
        incr_cur ();
        emit_incr Token.DOTDOT
      ) else
        raise (Lex_err ("unexpected character '.'", span_from start))

    | '<' -> emit_incr Token.LT
    | '>' -> emit_incr Token.GT
    | '+' -> emit_incr Token.PLUS
    | '*' -> emit_incr Token.STAR
    | '/' -> emit_incr Token.SLASH

    | '"' ->
      incr_cur ();
      let buf = Buffer.create 16 in
      let rec loop () =
        if !pos >= String.length src then
          raise (Lex_err ("unterminated text literal", span_from start));
        match src.[!pos] with
        | '\n' ->
          raise (Lex_err ("literal newline in text literal", span_from start))
        | '"' -> ()
        | '\\' ->
          incr_cur ();
          if !pos >= String.length src then
            raise (Lex_err ("unterminated text literal", span_from start));
          let decoded = match src.[!pos] with
            | '\\' -> '\\'
            | '"'  -> '"'
            | 'n'  -> '\n'
            | 't'  -> '\t'
            | bad  ->
              raise (Lex_err
                (Printf.sprintf "invalid escape sequence '\\%c'" bad,
                 span_from start))
          in
          Buffer.add_char buf decoded;
          incr_cur ();
          loop ()
        | c ->
          Buffer.add_char buf c;
          incr_cur ();
          loop ()
      in
      loop ();
      emit_incr (Token.TEXT_LIT (Buffer.contents buf))

    | c when Char.is_digit c ->
      let start_idx = !pos in
      while !pos + 1 < String.length src && Char.is_digit src.[!pos + 1] do
        incr_cur ()
      done;
      let num_str = String.sub src ~pos:start_idx ~len:(!pos - start_idx + 1) in
      let num = try int_of_string num_str with Failure _ -> raise (Lex_err ("integer literal out of bounds", span_from start)) in
      emit_incr (Token.NUM_LIT num);
      
    | c when Char.is_alpha c || Char.equal c '_' ->
      let word = read_word () in
      let tok = match word with
        | "fn" -> Token.KW_FN
        | "let" -> Token.KW_LET
        | "in" -> Token.KW_IN
        | "match" -> Token.KW_MATCH
        | "type" -> Token.KW_TYPE
        | "pile" -> Token.KW_PILE
        | "of" -> Token.KW_OF
        | "visibility" -> Token.KW_VISIBILITY
        | "mod" -> Token.KW_MOD
        | _ -> if Char.is_uppercase c then Token.TYPE_IDENT word else Token.VALUE_IDENT word
      in emit_incr tok

    | c when Char.to_int c > 127 -> raise (Lex_err ("unexpected non-ASCII character", span_from start))

    | c -> raise (Lex_err (Printf.sprintf "unexpected character %C" c, span_from start))
  in 

  let scan_one () =
    match src.[!pos] with
    | ' ' | '\t' | '\r' -> incr_cur ()
    | '\n' -> incr line; col := 1; incr pos
    | _ -> scan_token ()
  in

  try
    while !pos < String.length src do scan_one () done;
    emit Token.EOF (span_from (mark ()));
    Ok (List.rev !tokens)
  with Lex_err (msg, sp) -> Error (Load_error.make Load_error.Lex sp msg)