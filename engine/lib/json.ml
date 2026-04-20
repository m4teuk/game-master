type t =
  | J_null
  | J_bool of bool
  | J_num of int
  | J_text of string
  | J_array of t list
  | J_object of (string * t) list

(* ---------------------------------------------------------------- *)
(* Parser                                                             *)
(* ---------------------------------------------------------------- *)

type parser_state = {
  src : string;
  mutable pos : int;
}

let err (p : parser_state) msg =
  Error (Printf.sprintf "json: pos %d: %s" p.pos msg)

let eof p = p.pos >= String.length p.src
let peek p = if eof p then '\x00' else p.src.[p.pos]
let advance p = p.pos <- p.pos + 1

let rec skip_ws p =
  if not (eof p) then
    match p.src.[p.pos] with
    | ' ' | '\t' | '\n' | '\r' -> advance p; skip_ws p
    | _ -> ()

let expect p c =
  if peek p = c then (advance p; Ok ())
  else err p (Printf.sprintf "expected '%c'" c)

let parse_keyword p kw value =
  let n = String.length kw in
  if p.pos + n <= String.length p.src
     && String.equal (String.sub p.src p.pos n) kw
  then (p.pos <- p.pos + n; Ok value)
  else err p (Printf.sprintf "expected '%s'" kw)

let parse_string p : (string, string) result =
  if peek p <> '"' then err p "expected '\"'"
  else begin
    advance p;
    let buf = Buffer.create 16 in
    let rec loop () =
      if eof p then err p "unterminated string"
      else
        let c = p.src.[p.pos] in
        if Char.equal c '"' then (advance p; Ok (Buffer.contents buf))
        else if Char.equal c '\\' then begin
          advance p;
          if eof p then err p "unterminated escape"
          else
            let esc = p.src.[p.pos] in
            advance p;
            match esc with
            | '"'  -> Buffer.add_char buf '"';  loop ()
            | '\\' -> Buffer.add_char buf '\\'; loop ()
            | '/'  -> Buffer.add_char buf '/';  loop ()
            | 'n'  -> Buffer.add_char buf '\n'; loop ()
            | 't'  -> Buffer.add_char buf '\t'; loop ()
            | 'r'  -> Buffer.add_char buf '\r'; loop ()
            | c    -> err p (Printf.sprintf "bad escape '\\%c'" c)
        end else begin
          Buffer.add_char buf c;
          advance p;
          loop ()
        end
    in
    loop ()
  end

let parse_num p : (int, string) result =
  let start = p.pos in
  if peek p = '-' then advance p;
  let digit_start = p.pos in
  while not (eof p) && (match p.src.[p.pos] with '0'..'9' -> true | _ -> false)
  do advance p done;
  if p.pos = digit_start then err p "expected digit"
  else if peek p = '.' || peek p = 'e' || peek p = 'E' then
    err p "fractional/exponent literals not supported (integers only)"
  else
    let s = String.sub p.src start (p.pos - start) in
    match int_of_string_opt s with
    | Some n -> Ok n
    | None -> err p (Printf.sprintf "could not parse number '%s'" s)

let rec parse_value p : (t, string) result =
  skip_ws p;
  match peek p with
  | '"' ->
    (match parse_string p with
     | Ok s -> Ok (J_text s)
     | Error e -> Error e)
  | '{' -> parse_object p
  | '[' -> parse_array p
  | 't' -> parse_keyword p "true"  (J_bool true)
  | 'f' -> parse_keyword p "false" (J_bool false)
  | 'n' -> parse_keyword p "null"  J_null
  | '-' | '0'..'9' ->
    (match parse_num p with
     | Ok n -> Ok (J_num n)
     | Error e -> Error e)
  | '\x00' -> err p "unexpected end of input"
  | c -> err p (Printf.sprintf "unexpected character '%c'" c)

and parse_object p =
  match expect p '{' with
  | Error e -> Error e
  | Ok () ->
    skip_ws p;
    if peek p = '}' then (advance p; Ok (J_object []))
    else
      let rec loop acc =
        skip_ws p;
        match parse_string p with
        | Error e -> Error e
        | Ok key ->
          skip_ws p;
          (match expect p ':' with
           | Error e -> Error e
           | Ok () ->
             match parse_value p with
             | Error e -> Error e
             | Ok v ->
               skip_ws p;
               match peek p with
               | ',' -> advance p; loop ((key, v) :: acc)
               | '}' -> advance p; Ok (J_object (List.rev ((key, v) :: acc)))
               | c -> err p (Printf.sprintf "expected ',' or '}', got '%c'" c))
      in
      loop []

and parse_array p =
  match expect p '[' with
  | Error e -> Error e
  | Ok () ->
    skip_ws p;
    if peek p = ']' then (advance p; Ok (J_array []))
    else
      let rec loop acc =
        match parse_value p with
        | Error e -> Error e
        | Ok v ->
          skip_ws p;
          match peek p with
          | ',' -> advance p; loop (v :: acc)
          | ']' -> advance p; Ok (J_array (List.rev (v :: acc)))
          | c -> err p (Printf.sprintf "expected ',' or ']', got '%c'" c)
      in
      loop []

let parse (s : string) : (t, string) result =
  let p = { src = s; pos = 0 } in
  match parse_value p with
  | Error e -> Error e
  | Ok v ->
    skip_ws p;
    if p.pos <> String.length s then
      Error (Printf.sprintf "json: pos %d: trailing content" p.pos)
    else Ok v

(* ---------------------------------------------------------------- *)
(* Formatter                                                          *)
(* ---------------------------------------------------------------- *)

let escape_string buf s =
  Buffer.add_char buf '"';
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | c when Char.code c < 0x20 ->
      Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buf c) s;
  Buffer.add_char buf '"'

let rec add_compact buf = function
  | J_null -> Buffer.add_string buf "null"
  | J_bool true -> Buffer.add_string buf "true"
  | J_bool false -> Buffer.add_string buf "false"
  | J_num n -> Buffer.add_string buf (string_of_int n)
  | J_text s -> escape_string buf s
  | J_array xs ->
    Buffer.add_char buf '[';
    List.iteri (fun i x ->
      if i > 0 then Buffer.add_char buf ',';
      add_compact buf x) xs;
    Buffer.add_char buf ']'
  | J_object pairs ->
    Buffer.add_char buf '{';
    List.iteri (fun i (k, v) ->
      if i > 0 then Buffer.add_char buf ',';
      escape_string buf k;
      Buffer.add_char buf ':';
      add_compact buf v) pairs;
    Buffer.add_char buf '}'

let rec add_pretty buf ~indent = function
  | (J_null | J_bool _ | J_num _ | J_text _) as j -> add_compact buf j
  | J_array [] -> Buffer.add_string buf "[]"
  | J_array xs ->
    Buffer.add_char buf '[';
    let inner = indent ^ "  " in
    List.iteri (fun i x ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf '\n';
      Buffer.add_string buf inner;
      add_pretty buf ~indent:inner x) xs;
    Buffer.add_char buf '\n';
    Buffer.add_string buf indent;
    Buffer.add_char buf ']'
  | J_object [] -> Buffer.add_string buf "{}"
  | J_object pairs ->
    Buffer.add_char buf '{';
    let inner = indent ^ "  " in
    List.iteri (fun i (k, v) ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf '\n';
      Buffer.add_string buf inner;
      escape_string buf k;
      Buffer.add_string buf ": ";
      add_pretty buf ~indent:inner v) pairs;
    Buffer.add_char buf '\n';
    Buffer.add_string buf indent;
    Buffer.add_char buf '}'

let to_string ?(pretty = false) (t : t) : string =
  let buf = Buffer.create 64 in
  if pretty then add_pretty buf ~indent:"" t else add_compact buf t;
  Buffer.contents buf
