(* Shared CLI helpers: seed parsing/generation, option parsing, error
   formatting. Used by both binaries. *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

(* 32 hex chars → 16 bytes. Raises [Failure] on bad input. *)
let parse_seed_hex (s : string) : bytes =
  if String.length s <> 32 then
    failwith "seed must be exactly 32 hex chars (16 bytes)";
  let digit = function
    | '0'..'9' as c -> Char.code c - Char.code '0'
    | 'a'..'f' as c -> 10 + Char.code c - Char.code 'a'
    | 'A'..'F' as c -> 10 + Char.code c - Char.code 'A'
    | c -> failwith (Printf.sprintf "bad hex char: %c" c)
  in
  let b = Bytes.create 16 in
  for i = 0 to 15 do
    let hi = digit s.[i * 2] and lo = digit s.[i * 2 + 1] in
    Bytes.set b i (Char.chr ((hi lsl 4) lor lo))
  done;
  b

let random_seed () : bytes =
  (* self_init is coarse but adequate; the engine treats the seed as
     opaque 128-bit material. *)
  Random.self_init ();
  let b = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set b i (Char.chr (Random.int 256))
  done;
  b

let parse_option_entry (schema : Engine.option_field list) (entry : string)
    : (string * Engine.option_value, string) result =
  match String.index_opt entry '=' with
  | None ->
    Error (Printf.sprintf "missing '=' in option '%s'" entry)
  | Some i ->
    let k = String.trim (String.sub entry 0 i) in
    let v = String.trim (String.sub entry (i + 1) (String.length entry - i - 1)) in
    match List.find_opt (fun (f : Engine.option_field) -> f.name = k) schema with
    | None -> Error (Printf.sprintf "unknown option field '%s'" k)
    | Some f ->
      (match f.ty with
       | OT_num ->
         (match int_of_string_opt v with
          | Some n -> Ok (k, Engine.OV_num n)
          | None -> Error (Printf.sprintf "option '%s' expects a number, got '%s'" k v))
       | OT_text -> Ok (k, Engine.OV_text v)
       | OT_enum variants ->
         if List.mem v variants then Ok (k, Engine.OV_enum v)
         else Error (Printf.sprintf "option '%s' must be one of {%s}"
                       k (String.concat " | " variants)))

(* Parse "k=v,k=v" into a list of option bindings, reporting the first
   error. *)
let parse_options_csv (schema : Engine.option_field list) (s : string)
    : ((string * Engine.option_value) list, string) result =
  if String.trim s = "" then Ok []
  else
    let entries =
      String.split_on_char ',' s
      |> List.map String.trim
      |> List.filter (fun e -> e <> "")
    in
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | e :: rest ->
        match parse_option_entry schema e with
        | Ok kv -> go (kv :: acc) rest
        | Error msg -> Error msg
    in
    go [] entries

let parse_players_csv (s : string) : string list =
  String.split_on_char ',' s
  |> List.map String.trim
  |> List.filter (fun p -> p <> "")

(* Resolve a host spec that may be either a dotted-quad / IPv6 literal
   or a hostname (including "localhost"). We ask getaddrinfo for any
   IPv4 stream address and take the first. *)
let resolve_address (host : string) : Unix.inet_addr =
  match Unix.getaddrinfo host ""
          [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] with
  | { ai_addr = ADDR_INET (a, _); _ } :: _ -> a
  | _ ->
    failwith (Printf.sprintf "cannot resolve address '%s'" host)

let setup_error_to_string : Engine.setup_error -> string = function
  | Invalid_options m -> "invalid options: " ^ m
  | Invalid_seed m -> "invalid seed: " ^ m
  | Invalid_players m -> "invalid players: " ^ m
  | Setup_rejected m -> "setup rejected: " ^ m
  | Setup_fatal m -> "setup fatal: " ^ m

let load_parsed ~source_name (src : string)
    : (Engine.parsed, string list) result =
  match Engine.parse ~source_name src with
  | Ok p -> Ok p
  | Error es -> Error (List.map Engine.Error.to_string es)
