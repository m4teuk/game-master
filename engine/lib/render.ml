(* ---------------------------------------------------------------- *)
(* Text format — uniform named ADT ([Name(field=value, …)])          *)
(* ---------------------------------------------------------------- *)

(* Per the design decision: every ctor with fields renders as
   [Name(f1=v1, f2=v2, …)], including stdlib single-field variants
   like [Ok(value=…)], [Err(error=…)], [Contents(items=[…])]. Zero
   policy exceptions — purely structural.

   Opaque / non-serializable values shouldn't reach these entry
   points (typechecker rules them out of Action/Outcome fields), but
   we render them as [<kind>] sentinels rather than raising, so an
   engine bug surfaces visibly instead of crashing. *)

let rec value_to_text (v : Value.t) : string =
  match v with
  | V_num n -> string_of_int n
  | V_text s -> Printf.sprintf "%S" s
  | V_player p -> Printf.sprintf "%S" p
  | V_unit -> "Unit"
  | V_ctor { name; fields = [] } -> name
  | V_ctor { name; fields } ->
    Printf.sprintf "%s(%s)" name
      (String.concat ", "
         (List.map (fun (fn, fv) ->
            Printf.sprintf "%s=%s" fn (value_to_text fv)) fields))
  | V_tuple vs ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map value_to_text vs))
  | V_list vs ->
    Printf.sprintf "[%s]" (String.concat ", " (List.map value_to_text vs))
  | V_pile_ref { name; keys = [] } -> Printf.sprintf "<pile %s>" name
  | V_pile_ref { name; keys } ->
    Printf.sprintf "<pile %s(%s)>" name
      (String.concat ", " (List.map value_to_text keys))
  | V_fn _ -> "<fn>"
  | V_builtin n -> Printf.sprintf "<builtin %s>" n
  | V_pile_ctor { name; arity } -> Printf.sprintf "<pile_ctor %s/%d>" name arity
  | V_partial { arity; _ } -> Printf.sprintf "<partial/%d>" arity
  | V_state _ -> "<state>"
  | V_view _ -> "<view>"
  | V_rng -> "<rng>"

(* Action rendering prefixes with the acting player so the broadcast
   form is self-contained — the log/broadcast can be read without
   out-of-band context. The parser in [text_to_action] accepts both
   forms (with or without prefix). *)
let action_to_text (_link : Link.t) (v : Value.t)
    (p : Value.player_id) : string =
  Printf.sprintf "%s: %s" p (value_to_text v)

let outcome_to_text (_link : Link.t) (v : Value.t) (_p : Value.player_id) : string =
  value_to_text v

(* [view_to_text]: labeled blocks per stdlib §14 "minimal formatting".
   Authors wanting a prettier surface override this function; the
   default exists to keep unfamiliar games playable in the CLI. *)
let view_to_text (_link : Link.t) (view : Value.view)
    (_p : Value.player_id) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "Config:\n  ";
  Buffer.add_string buf (value_to_text view.view_config);
  Buffer.add_char buf '\n';

  if view.view_player_dicts <> [] then begin
    Buffer.add_string buf "\nPlayer dicts:\n";
    List.iter (fun (pid, dict) ->
      Buffer.add_string buf (Printf.sprintf "  %s: %s\n"
                               pid (value_to_text dict)))
      view.view_player_dicts
  end;

  if view.view_piles <> [] then begin
    Buffer.add_string buf "\nPiles:\n";
    List.iter (fun (vp : Value.view_pile) ->
      let key_str =
        if vp.vp_keys = [] then ""
        else
          "(" ^ String.concat ", " (List.map value_to_text vp.vp_keys) ^ ")"
      in
      Buffer.add_string buf
        (Printf.sprintf "  %s%s: %s\n"
           vp.vp_name key_str (value_to_text vp.vp_value)))
      view.view_piles
  end;
  Buffer.contents buf

(* ---------------------------------------------------------------- *)
(* text_to_action — recursive-descent parser for the printed form    *)
(* ---------------------------------------------------------------- *)

(* Grammar of the text form (round-trips with [value_to_text]):

     value: NUM_LIT or TEXT_LIT
     value: IDENT                             (nullary ctor or Unit)
     value: IDENT "(" field { "," field } ")" (ctor with fields)
     value: "(" value "," value { "," value } ")"  (tuple, arity >= 2)
     value: "[" [ value { "," value } ] "]"   (list)
     field: IDENT "=" value

   NUM_LIT: optional "-" then digits.
   TEXT_LIT: "..." with the same escape set as JSON.
   IDENT: [A-Za-z_][A-Za-z0-9_]*.

   At the top level, [text_to_action] also strips an optional
   "<player>: " prefix so users can paste [action_to_text]'s output
   directly. The engine already knows the acting player, so the
   prefix is only a UX nicety. *)

type tp = { src : string; mutable pos : int }

let tp_err (p : tp) (msg : string) =
  Error (Printf.sprintf "pos %d: %s" p.pos msg)

let tp_eof p = p.pos >= String.length p.src
let tp_peek p = if tp_eof p then '\x00' else p.src.[p.pos]
let tp_advance p = p.pos <- p.pos + 1

let rec tp_skip_ws p =
  if not (tp_eof p) then
    match p.src.[p.pos] with
    | ' ' | '\t' | '\n' | '\r' -> tp_advance p; tp_skip_ws p
    | _ -> ()

let tp_expect p c =
  tp_skip_ws p;
  if tp_peek p = c then (tp_advance p; Ok ())
  else tp_err p (Printf.sprintf "expected '%c'" c)

let is_id_start c =
  match c with 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false

let is_id_cont c =
  match c with 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false

let tp_parse_ident p : (string, string) result =
  tp_skip_ws p;
  let start = p.pos in
  if tp_eof p || not (is_id_start p.src.[p.pos]) then
    tp_err p "expected identifier"
  else begin
    while not (tp_eof p) && is_id_cont p.src.[p.pos] do tp_advance p done;
    Ok (String.sub p.src start (p.pos - start))
  end

let tp_parse_num p : (int, string) result =
  tp_skip_ws p;
  let start = p.pos in
  if tp_peek p = '-' then tp_advance p;
  let digit_start = p.pos in
  while not (tp_eof p)
        && (match p.src.[p.pos] with '0'..'9' -> true | _ -> false)
  do tp_advance p done;
  if p.pos = digit_start then tp_err p "expected number"
  else
    let s = String.sub p.src start (p.pos - start) in
    match int_of_string_opt s with
    | Some n -> Ok n
    | None -> tp_err p (Printf.sprintf "bad number '%s'" s)

let tp_parse_text p : (string, string) result =
  tp_skip_ws p;
  if tp_peek p <> '"' then tp_err p "expected '\"'"
  else begin
    tp_advance p;
    let buf = Buffer.create 16 in
    let rec loop () =
      if tp_eof p then tp_err p "unterminated string"
      else
        let c = p.src.[p.pos] in
        if Char.equal c '"' then (tp_advance p; Ok (Buffer.contents buf))
        else if Char.equal c '\\' then begin
          tp_advance p;
          if tp_eof p then tp_err p "unterminated escape"
          else
            let esc = p.src.[p.pos] in
            tp_advance p;
            match esc with
            | '"'  -> Buffer.add_char buf '"';  loop ()
            | '\\' -> Buffer.add_char buf '\\'; loop ()
            | 'n'  -> Buffer.add_char buf '\n'; loop ()
            | 't'  -> Buffer.add_char buf '\t'; loop ()
            | 'r'  -> Buffer.add_char buf '\r'; loop ()
            | c    -> tp_err p (Printf.sprintf "bad escape '\\%c'" c)
        end else begin
          Buffer.add_char buf c;
          tp_advance p;
          loop ()
        end
    in
    loop ()
  end

(* Optional "<ident>:" prefix. Consumes it on success; rewinds on miss
   so unprefixed input still parses. *)
let tp_skip_optional_player_prefix (p : tp) : unit =
  let saved = p.pos in
  tp_skip_ws p;
  let ident_start = p.pos in
  if not (tp_eof p) && is_id_start p.src.[p.pos] then begin
    while not (tp_eof p) && is_id_cont p.src.[p.pos] do tp_advance p done;
    tp_skip_ws p;
    if p.pos > ident_start && tp_peek p = ':' then
      tp_advance p  (* keep the consumed prefix *)
    else
      p.pos <- saved  (* not a prefix; restore *)
  end else
    p.pos <- saved

let rec tp_parse_value p (link : Link.t) ~(expected : Types.ty)
    : (Value.t, string) result =
  tp_skip_ws p;
  match expected with
  | T_num ->
    (match tp_parse_num p with
     | Ok n -> Ok (Value.V_num n) | Error e -> Error e)
  | T_text ->
    (match tp_parse_text p with
     | Ok s -> Ok (Value.V_text s) | Error e -> Error e)
  | T_player_id ->
    (match tp_parse_text p with
     | Ok s -> Ok (Value.V_player s) | Error e -> Error e)
  | T_unit ->
    (match tp_parse_ident p with
     | Ok "Unit" -> Ok Value.V_unit
     | Ok other ->
       tp_err p (Printf.sprintf "expected 'Unit', got '%s'" other)
     | Error e -> Error e)
  | T_tuple ts -> tp_parse_tuple p link ts
  | T_list elem_ty -> tp_parse_list p link elem_ty
  | T_user name -> tp_parse_user_ctor p link name
  | T_options -> tp_parse_user_ctor p link "Options"
  | T_result (ok_ty, err_ty) ->
    tp_parse_tagged p link
      [("Ok", [("value", ok_ty)]); ("Err", [("error", err_ty)])]
  | T_game_status outcome_ty ->
    tp_parse_tagged p link
      [("Ongoing", []); ("Ended", [("outcome", outcome_ty)])]
  | T_pile_view item_ty ->
    tp_parse_tagged p link
      [("Contents", [("items", Types.T_list item_ty)]);
       ("Size",     [("n", Types.T_num)]);
       ("Masked",   [])]
  | T_fn _ | T_rng | T_state | T_view | T_pile_ref _ | T_var _ ->
    tp_err p
      (Printf.sprintf "cannot parse a value of type '%s'"
         (Types.string_of_ty expected))

and tp_parse_tuple p link ts =
  match tp_expect p '(' with
  | Error e -> Error e
  | Ok () ->
    let rec go acc = function
      | [] -> tp_err p "tuple must have arity >= 2"
      | [t] ->
        (match tp_parse_value p link ~expected:t with
         | Error e -> Error e
         | Ok v ->
           (match tp_expect p ')' with
            | Error e -> Error e
            | Ok () -> Ok (Value.V_tuple (List.rev (v :: acc)))))
      | t :: rest ->
        (match tp_parse_value p link ~expected:t with
         | Error e -> Error e
         | Ok v ->
           (match tp_expect p ',' with
            | Error e -> Error e
            | Ok () -> go (v :: acc) rest))
    in
    go [] ts

and tp_parse_list p link elem_ty =
  match tp_expect p '[' with
  | Error e -> Error e
  | Ok () ->
    tp_skip_ws p;
    if tp_peek p = ']' then (tp_advance p; Ok (Value.V_list []))
    else
      let rec loop acc =
        match tp_parse_value p link ~expected:elem_ty with
        | Error e -> Error e
        | Ok v ->
          tp_skip_ws p;
          match tp_peek p with
          | ',' -> tp_advance p; loop (v :: acc)
          | ']' -> tp_advance p; Ok (Value.V_list (List.rev (v :: acc)))
          | c -> tp_err p (Printf.sprintf "expected ',' or ']', got '%c'" c)
      in
      loop []

and tp_parse_user_ctor p link type_name =
  match tp_parse_ident p with
  | Error e -> Error e
  | Ok ctor_name ->
    (match Types.lookup_type link.env type_name with
     | Some (Types.TI_adt { ctors; _ }) ->
       (match List.find_opt (fun (c : Types.ctor_info) ->
            String.equal c.ctor_name ctor_name) ctors with
        | None ->
          tp_err p
            (Printf.sprintf
               "unknown constructor '%s' for type '%s'"
               ctor_name type_name)
        | Some info ->
          if info.fields = [] then
            Ok (Value.V_ctor { name = ctor_name; fields = [] })
          else
            tp_parse_ctor_fields p link ctor_name info.fields)
     | _ ->
       tp_err p
         (Printf.sprintf "type '%s' is not a user ADT" type_name))

and tp_parse_tagged p link choices =
  match tp_parse_ident p with
  | Error e -> Error e
  | Ok ctor_name ->
    (match List.assoc_opt ctor_name choices with
     | None ->
       tp_err p
         (Printf.sprintf
            "unknown constructor '%s' (expected one of: %s)"
            ctor_name (String.concat ", " (List.map fst choices)))
     | Some decl_fields ->
       if decl_fields = [] then
         Ok (Value.V_ctor { name = ctor_name; fields = [] })
       else
         tp_parse_ctor_fields p link ctor_name decl_fields)

and tp_parse_ctor_fields p link ctor_name decl_fields =
  match tp_expect p '(' with
  | Error e -> Error e
  | Ok () ->
    let rec collect seen =
      tp_skip_ws p;
      match tp_parse_ident p with
      | Error e -> Error e
      | Ok fname ->
        (match List.assoc_opt fname decl_fields with
         | None ->
           tp_err p
             (Printf.sprintf
                "constructor '%s' has no field '%s'" ctor_name fname)
         | Some fty ->
           match tp_expect p '=' with
           | Error e -> Error e
           | Ok () ->
             match tp_parse_value p link ~expected:fty with
             | Error e -> Error e
             | Ok fv ->
               tp_skip_ws p;
               let seen' = (fname, fv) :: seen in
               match tp_peek p with
               | ',' -> tp_advance p; collect seen'
               | ')' ->
                 tp_advance p;
                 (* Assemble in declaration order; error on missing. *)
                 let rec build acc = function
                   | [] ->
                     Ok (Value.V_ctor
                           { name = ctor_name; fields = List.rev acc })
                   | (dn, _) :: rest ->
                     (match List.assoc_opt dn seen' with
                      | Some v -> build ((dn, v) :: acc) rest
                      | None ->
                        Error (Printf.sprintf
                                 "missing field '%s' for '%s'"
                                 dn ctor_name))
                 in
                 build [] decl_fields
               | c ->
                 tp_err p (Printf.sprintf
                             "expected ',' or ')', got '%c'" c))
    in
    collect []

(* Describe a ctor for user-facing help. *)
let describe_ctor (info : Types.ctor_info) : string =
  if info.fields = [] then info.ctor_name
  else
    Printf.sprintf "%s(%s)" info.ctor_name
      (String.concat ", "
         (List.map (fun (fn, fty) ->
            Printf.sprintf "%s=<%s>" fn (Types.string_of_ty fty))
            info.fields))

let action_help (link : Link.t) : string =
  match Types.lookup_type link.env link.action_type with
  | Some (Types.TI_adt { ctors; _ }) ->
    Printf.sprintf "Available %s values:\n  %s"
      link.action_type
      (String.concat "\n  " (List.map describe_ctor ctors))
  | _ -> "(no Action type registered)"

let text_to_action (link : Link.t) (input : string)
    ~(view : Value.view) ~(player : Value.player_id)
    : (Value.t, string) result =
  let _ = (view, player) in
  let p = { src = input; pos = 0 } in
  tp_skip_optional_player_prefix p;
  match tp_parse_value p link ~expected:(Types.T_user link.action_type) with
  | Error msg ->
    Error (Printf.sprintf "%s.\n%s" msg (action_help link))
  | Ok v ->
    tp_skip_ws p;
    if p.pos <> String.length input then
      Error (Printf.sprintf
               "trailing content after action at pos %d.\n%s"
               p.pos (action_help link))
    else
      Ok v

(* ---------------------------------------------------------------- *)
(* JSON format — canonical tagged union                              *)
(* ---------------------------------------------------------------- *)

(* [V_unit] gets the uniform [{"tag": "Unit"}] form so JSON parsing
   is always object-shaped when a ctor is expected. Primitives
   (Num, Text, PlayerId) serialize bare.

   [V_pile_ref] and the opaque engine types never appear in
   serializable positions (typechecker forbids them in Action/Outcome
   fields, and [view_to_json] emits piles structurally); we raise
   [Invalid_argument] if one does reach here — same policy as
   [Value.equal]. *)

let rec value_to_json (v : Value.t) : Json.t =
  match v with
  | V_num n -> J_num n
  | V_text s -> J_text s
  | V_player p -> J_text p
  | V_unit -> J_object [("tag", J_text "Unit")]
  | V_ctor { name; fields = [] } ->
    J_object [("tag", J_text name)]
  | V_ctor { name; fields } ->
    J_object (("tag", J_text name)
              :: List.map (fun (fn, fv) -> (fn, value_to_json fv)) fields)
  | V_tuple vs -> J_array (List.map value_to_json vs)
  | V_list vs -> J_array (List.map value_to_json vs)
  | V_pile_ref _ | V_fn _ | V_builtin _ | V_pile_ctor _ | V_partial _
  | V_state _ | V_view _ | V_rng ->
    invalid_arg "Render.value_to_json: value is not serializable"

let action_to_json (_link : Link.t) (v : Value.t) (_p : Value.player_id) : string =
  Json.to_string (value_to_json v)

let outcome_to_json (_link : Link.t) (v : Value.t) (_p : Value.player_id) : string =
  Json.to_string (value_to_json v)

let view_to_json (_link : Link.t) (view : Value.view)
    (_p : Value.player_id) : string =
  let pile_to_json (vp : Value.view_pile) : Json.t =
    J_object [
      ("name", J_text vp.vp_name);
      ("keys", J_array (List.map value_to_json vp.vp_keys));
      ("value", value_to_json vp.vp_value);
    ]
  in
  let dict_to_json (pid, dict) : Json.t =
    J_object [("player", J_text pid); ("dict", value_to_json dict)]
  in
  let obj : Json.t = J_object [
    ("config", value_to_json view.view_config);
    ("player_dicts", J_array (List.map dict_to_json view.view_player_dicts));
    ("piles", J_array (List.map pile_to_json view.view_piles));
    ("roster",
     J_array (List.map (fun p -> Json.J_text p) view.view_roster));
  ] in
  Json.to_string obj

(* ---------------------------------------------------------------- *)
(* JSON input — type-directed deserialization                        *)
(* ---------------------------------------------------------------- *)

(* Walks a [Types.ty] and a [Json.t] in lockstep, producing a [Value.t]
   or a parse error. Tagged builtins (Result, GameStatus, PileView,
   Options) have their own handlers so we don't rely on the [T_var]-
   bearing seeded ctors, which would misbehave without unification at
   this layer. *)

let rec json_to_value (link : Link.t) ~(expected_ty : Types.ty)
    (j : Json.t) : (Value.t, string) result =
  match expected_ty, j with
  | T_num, J_num n -> Ok (V_num n)
  | T_num, _ -> Error "expected Num (JSON integer)"

  | T_text, J_text s -> Ok (V_text s)
  | T_text, _ -> Error "expected Text (JSON string)"

  | T_player_id, J_text s -> Ok (V_player s)
  | T_player_id, _ -> Error "expected PlayerId (JSON string)"

  | T_unit, J_object [("tag", J_text "Unit")] -> Ok V_unit
  | T_unit, _ -> Error "expected Unit (JSON {\"tag\":\"Unit\"})"

  | T_tuple ts, J_array js when List.length ts = List.length js ->
    let rec go acc ts js = match ts, js with
      | [], [] -> Ok (Value.V_tuple (List.rev acc))
      | t :: ts', j :: js' ->
        (match json_to_value link ~expected_ty:t j with
         | Error e -> Error e
         | Ok v -> go (v :: acc) ts' js')
      | _ -> Error "tuple arity mismatch (internal)"
    in
    go [] ts js
  | T_tuple _, _ -> Error "expected tuple (JSON array of matching arity)"

  | T_list elem_ty, J_array js ->
    let rec go acc = function
      | [] -> Ok (Value.V_list (List.rev acc))
      | j :: rest ->
        (match json_to_value link ~expected_ty:elem_ty j with
         | Error e -> Error e
         | Ok v -> go (v :: acc) rest)
    in
    go [] js
  | T_list _, _ -> Error "expected list (JSON array)"

  | T_user name, J_object pairs ->
    convert_user_ctor link name pairs
  | T_user _, _ -> Error "expected constructor (JSON object with \"tag\")"

  | T_options, J_object pairs ->
    (* Synthesized type-name is always "Options". *)
    convert_user_ctor link "Options" pairs
  | T_options, _ -> Error "expected Options (JSON object)"

  | T_result (ok_ty, err_ty), J_object pairs ->
    convert_stdlib_ctor link pairs
      ~choices:[("Ok", [("value", ok_ty)]); ("Err", [("error", err_ty)])]

  | T_game_status outcome_ty, J_object pairs ->
    convert_stdlib_ctor link pairs
      ~choices:[("Ongoing", []); ("Ended", [("outcome", outcome_ty)])]

  | T_pile_view item_ty, J_object pairs ->
    convert_stdlib_ctor link pairs
      ~choices:[
        ("Contents", [("items", Types.T_list item_ty)]);
        ("Size",     [("n", Types.T_num)]);
        ("Masked",   []);
      ]

  | T_result _, _ | T_game_status _, _ | T_pile_view _, _ ->
    Error "expected tagged constructor (JSON object with \"tag\")"

  | (T_fn _ | T_rng | T_state | T_view | T_pile_ref _ | T_var _), _ ->
    Error (Printf.sprintf
             "cannot deserialize value of non-serializable type '%s'"
             (Types.string_of_ty expected_ty))

and convert_user_ctor (link : Link.t) (type_name : string)
    (pairs : (string * Json.t) list) : (Value.t, string) result =
  match List.assoc_opt "tag" pairs with
  | None -> Error (Printf.sprintf "missing \"tag\" for type '%s'" type_name)
  | Some (Json.J_text tag) ->
    (match Types.lookup_type link.env type_name with
     | Some (Types.TI_adt { ctors; _ }) ->
       (match List.find_opt (fun (c : Types.ctor_info) ->
           String.equal c.ctor_name tag) ctors with
        | None ->
          Error (Printf.sprintf
                   "unknown constructor '%s' for type '%s'" tag type_name)
        | Some info ->
          build_ctor_fields link tag info.fields pairs)
     | _ ->
       Error (Printf.sprintf
                "type '%s' is not a user ADT (internal)" type_name))
  | Some _ -> Error "\"tag\" must be a string"

and convert_stdlib_ctor (link : Link.t) (pairs : (string * Json.t) list)
    ~(choices : (string * (string * Types.ty) list) list)
    : (Value.t, string) result =
  match List.assoc_opt "tag" pairs with
  | None -> Error "missing \"tag\" for tagged constructor"
  | Some (Json.J_text tag) ->
    (match List.assoc_opt tag choices with
     | None ->
       Error (Printf.sprintf
                "constructor '%s' is not valid here (expected one of: %s)"
                tag (String.concat ", " (List.map fst choices)))
     | Some decl_fields ->
       build_ctor_fields link tag decl_fields pairs)
  | Some _ -> Error "\"tag\" must be a string"

and build_ctor_fields (link : Link.t) (ctor_name : string)
    (decl_fields : (string * Types.ty) list)
    (pairs : (string * Json.t) list)
    : (Value.t, string) result =
  let rec go acc = function
    | [] -> Ok (Value.V_ctor { name = ctor_name; fields = List.rev acc })
    | (fname, fty) :: rest ->
      (match List.assoc_opt fname pairs with
       | None ->
         Error (Printf.sprintf
                  "missing field '%s' for constructor '%s'" fname ctor_name)
       | Some fj ->
         match json_to_value link ~expected_ty:fty fj with
         | Error e ->
           Error (Printf.sprintf "%s.%s: %s" ctor_name fname e)
         | Ok v -> go ((fname, v) :: acc) rest)
  in
  go [] decl_fields

let json_to_action (link : Link.t) (input : string)
    ~(view : Value.view) ~(player : Value.player_id)
    : (Value.t, string) result =
  let _ = (view, player) in
  match Json.parse input with
  | Error e -> Error e
  | Ok j ->
    json_to_value link ~expected_ty:(Types.T_user link.action_type) j
