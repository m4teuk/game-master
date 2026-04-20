(* Dev CLI for the .game engine.

   Subcommands:
     tokens FILE       Print the lexed tokens.
     ast FILE          Print the parsed AST.
     tast FILE         Run lex+parse+typecheck; print the typed AST.
     link FILE         Run all four passes; print the linked summary.
     check FILE        Quick health report — pass/fail per stage.
     pipeline FILE     Run all stages; dump the output of each in turn.
     session FILE ...  Interactive REPL (not yet implemented). *)

open Engine.Dev

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

let print_errors errs =
  Dump.dump_errors Format.err_formatter errs

let exit_on_errors stage errs =
  Format.eprintf "[%s] %d error(s):@." stage (List.length errs);
  print_errors errs;
  exit 1

(* ---------------------------------------------------------------- *)
(* Stage runners                                                      *)
(* ---------------------------------------------------------------- *)

let run_lex path =
  let src = read_file path in
  match Lexer.tokenize ~file:path src with
  | Ok toks -> toks
  | Error e -> exit_on_errors "lex" [e]

let run_parse path =
  let toks = run_lex path in
  match Parser.parse ~source_name:path toks with
  | Ok file -> file
  | Error e -> exit_on_errors "parse" [e]

let run_typecheck path =
  let file = run_parse path in
  match Typecheck.check file with
  | Ok (tf, env) -> (tf, env)
  | Error es -> exit_on_errors "typecheck" es

let run_link path =
  let (tf, env) = run_typecheck path in
  match Link.link tf env with
  | Ok l -> l
  | Error es -> exit_on_errors "link" es

(* ---------------------------------------------------------------- *)
(* Subcommands                                                        *)
(* ---------------------------------------------------------------- *)

let cmd_tokens path =
  let toks = run_lex path in
  Dump.dump_tokens Format.std_formatter toks

let cmd_ast path =
  let file = run_parse path in
  Dump.dump_ast Format.std_formatter file

let cmd_tast path =
  let (tf, _env) = run_typecheck path in
  Dump.dump_tast Format.std_formatter tf

let cmd_link path =
  let l = run_link path in
  Dump.dump_link Format.std_formatter l

let cmd_toplevel path =
  let l = run_link path in
  (* Runs [Interp.build_toplevel] with an empty stdlib — only fn
     closures, pile ctors, and top-level lets that use no stdlib will
     bind successfully. Proof-of-life for Phase 2 of the runtime. *)
  match Interp.build_toplevel l [] with
  | exception Value.Fatal msg ->
    Format.eprintf "[toplevel] Fatal: %s@." msg;
    exit 1
  | entries ->
    Format.printf "Toplevel env (%d entries):@." (List.length entries);
    List.iter (fun (name, v) ->
      Format.printf "  %-20s = %a@." name Dump.pp_value v)
      entries

let cmd_check path =
  let src = read_file path in
  let stage label result_show result_fail = function
    | Ok x ->
      Format.printf "%-12s ✓ %s@." label (result_show x); x
    | Error es ->
      Format.printf "%-12s ✗@." label;
      List.iter (fun e -> Format.printf "  %s@." (Load_error.to_string e)) es;
      result_fail ()
  in
  let toks = stage "lex"
    (fun ts -> Printf.sprintf "%d tokens" (List.length ts))
    (fun () -> exit 1)
    (Lexer.tokenize ~file:path src |> Result.map_error (fun e -> [e])) in
  let file = stage "parse"
    (fun (f : Ast.file) ->
       Printf.sprintf "%d declarations" (List.length f.decls))
    (fun () -> exit 1)
    (Parser.parse ~source_name:path toks |> Result.map_error (fun e -> [e])) in
  let (tf, env) = stage "typecheck"
    (fun (tf, _) ->
       Printf.sprintf "%d types, %d piles, %d fns, %d lets, %d options"
         (List.length (Typecheck.type_decls tf))
         (List.length (Typecheck.pile_decls tf))
         (List.length (Typecheck.top_fns tf))
         (List.length (Typecheck.top_lets tf))
         (List.length (Typecheck.options_decl tf)))
    (fun () -> exit 1)
    (Typecheck.check file) in
  let _link = stage "link"
    (fun (l : Link.t) ->
       Printf.sprintf "%d piles, %d option fields"
         (List.length l.pile_decls)
         (List.length l.options_schema))
    (fun () -> exit 1)
    (Link.link tf env) in
  ()

let cmd_pipeline path =
  let bar label =
    Format.printf "@.═══ %s ═══════════════════════════════@.@." label
  in
  bar "TOKENS"; cmd_tokens path;
  bar "AST"; cmd_ast path;
  bar "TYPED AST"; cmd_tast path;
  bar "LINK"; cmd_link path

(* ---------------------------------------------------------------- *)
(* Session REPL                                                       *)
(* ---------------------------------------------------------------- *)

(* Parses a 16-hex-char seed string into 16 bytes. Accepts exactly
   32 hex chars or padding if shorter. Used only by the dev REPL. *)
let parse_seed (s : string) : bytes =
  let s =
    if String.length s < 32 then
      s ^ String.make (32 - String.length s) '0'
    else if String.length s > 32 then
      String.sub s 0 32
    else s
  in
  let buf = Bytes.create 16 in
  for i = 0 to 15 do
    let hi = s.[i * 2] and lo = s.[i * 2 + 1] in
    let digit c =
      match c with
      | '0'..'9' -> Char.code c - Char.code '0'
      | 'a'..'f' -> 10 + Char.code c - Char.code 'a'
      | 'A'..'F' -> 10 + Char.code c - Char.code 'A'
      | _ -> 0
    in
    Bytes.set buf i (Char.chr ((digit hi lsl 4) lor digit lo))
  done;
  buf

let load_and_init path players seed_hex :
    (Engine.parsed * Engine.engine_state) option =
  let src = read_file path in
  match Engine.parse ~source_name:path src with
  | Error errs ->
    Format.eprintf "Parse/type/link failed:@.";
    List.iter (fun e ->
      Format.eprintf "  %s@." (Engine.Error.to_string e)) errs;
    None
  | Ok parsed ->
    let seed = parse_seed seed_hex in
    (match Engine.init_state parsed ~options:[] ~players ~seed with
     | Ok state -> Some (parsed, state)
     | Error err ->
       let msg = match err with
         | Engine.Invalid_options m -> "invalid options: " ^ m
         | Invalid_seed m -> "invalid seed: " ^ m
         | Invalid_players m -> "invalid players: " ^ m
         | Setup_rejected m -> "setup rejected: " ^ m
         | Setup_fatal m -> "setup fatal: " ^ m
       in
       Format.eprintf "init_state failed: %s@." msg;
       None)

(* Dev-only full state dump — bypasses view-masking and uses the typed
   value pretty-printer so engine state is readable. *)
let dump_full_state (s : Value.state) =
  Format.printf "State (full, no masking):@.";
  Format.printf "  config = %a@." Dump.pp_value s.state_config;
  Format.printf "  roster = [%s]@."
    (String.concat "; " s.state_roster);
  Format.printf "  player_dicts:@.";
  List.iter (fun (p, d) ->
    Format.printf "    %-10s = %a@." p Dump.pp_value d)
    s.state_player_dicts;
  Format.printf "  piles:@.";
  List.iter (fun (e : Value.pile_entry) ->
    let key_str =
      if e.pe_keys = [] then ""
      else
        "(" ^ String.concat ", "
                (List.map (fun v ->
                   Format.asprintf "%a" Dump.pp_value v) e.pe_keys)
        ^ ")"
    in
    Format.printf "    %s%s: [%s]@." e.pe_name key_str
      (String.concat ", "
         (List.map (fun v ->
            Format.asprintf "%a" Dump.pp_value v) e.pe_cards)))
    s.state_piles

let session_help () =
  print_endline "Commands:";
  print_endline "  view PLAYER             Show PLAYER's view (rendered text).";
  print_endline "  state                   Dump the full engine state (no masking).";
  print_endline "  act PLAYER JSON         Submit an action (JSON form).";
  print_endline "  status PLAYER           Show terminal status for PLAYER.";
  print_endline "  log                     Print the action log.";
  print_endline "  help                    Show this help.";
  print_endline "  quit                    Exit."

let cmd_session (args : string list) =
  let path, players, seed_hex =
    match args with
    | path :: players_csv :: seed :: _ ->
      (path, String.split_on_char ',' players_csv, seed)
    | path :: players_csv :: [] ->
      (path, String.split_on_char ',' players_csv,
       "0123456789abcdef0123456789abcdef")
    | _ ->
      prerr_endline
        "usage: cli session FILE PLAYERS_CSV [SEED_HEX32]";
      prerr_endline "  e.g.: cli session war.game alice,bob";
      exit 2
  in
  match load_and_init path players seed_hex with
  | None -> exit 1
  | Some (parsed, initial_es) ->
    Format.printf "Loaded %s. Players: %s. Type 'help' for commands.@."
      (Engine.source_name parsed) (String.concat ", " players);
    let es = ref initial_es in
    let run_cmd line =
      let trimmed = String.trim line in
      if trimmed = "" then ()
      else
        let words =
          (* Split on whitespace, preserving JSON that starts with { or [.
             Simple heuristic: first two words are command + player,
             rest (joined) is the action JSON. *)
          String.split_on_char ' ' trimmed
          |> List.filter (fun s -> s <> "")
        in
        match words with
        | ["help"] -> session_help ()
        | ["quit"] | ["exit"] -> raise Exit
        | ["log"] ->
          let entries = Engine.log !es in
          if entries = [] then print_endline "  (empty)"
          else
            (* [action_to_text] already embeds the acting player, so
               the log entry's [rendered] is self-describing. *)
            List.iteri (fun i (e : Engine.log_entry) ->
              Format.printf "  %3d. %s@." i e.rendered) entries
        | ["state"] -> dump_full_state (Engine.Dev.raw_state !es)
        | ["view"; p] ->
          let out = Engine.display parsed !es ~player:p in
          print_endline out
        | ["status"; p] ->
          (match Engine.status parsed !es ~player:p with
           | Ongoing -> print_endline "Ongoing"
           | Ended msg -> Format.printf "Ended: %s@." msg)
        | "act" :: p :: rest ->
          let input = String.concat " " rest in
          (match Engine.apply parsed !es ~player:p ~input with
           | Ok (new_es, rendered) ->
             es := new_es;
             Format.printf "  -> %s@." rendered
           | Error (Invalid m) -> Format.printf "Invalid: %s@." m
           | Error (Fatal m) -> Format.printf "Fatal: %s@." m)
        | _ ->
          Format.printf "Unrecognized: '%s'. 'help' for commands.@." trimmed
    in
    (try
       while true do
         print_string "> ";
         (try run_cmd (input_line stdin) with End_of_file -> raise Exit)
       done
     with Exit -> print_endline "bye.")

(* ---------------------------------------------------------------- *)
(* Dispatch                                                           *)
(* ---------------------------------------------------------------- *)

let usage () =
  prerr_endline "Usage: cli <subcommand> FILE [args...]";
  prerr_endline "Subcommands:";
  prerr_endline "  tokens FILE       Lex and dump the token stream.";
  prerr_endline "  ast FILE          Lex+parse and dump the parsed AST.";
  prerr_endline "  tast FILE         + typecheck and dump the typed AST.";
  prerr_endline "  link FILE         + link and dump the linked summary.";
  prerr_endline "  toplevel FILE     Run Interp.build_toplevel and dump the env.";
  prerr_endline "  check FILE        Quick pass/fail summary per stage.";
  prerr_endline "  pipeline FILE     Run every stage, dump each output.";
  prerr_endline "  session FILE PLAYERS_CSV [SEED_HEX32]";
  prerr_endline "                    Start an interactive REPL for a session.";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | _ :: "tokens"   :: path :: [] -> cmd_tokens path
  | _ :: "ast"      :: path :: [] -> cmd_ast path
  | _ :: "tast"     :: path :: [] -> cmd_tast path
  | _ :: "link"     :: path :: [] -> cmd_link path
  | _ :: "toplevel" :: path :: [] -> cmd_toplevel path
  | _ :: "check"    :: path :: [] -> cmd_check path
  | _ :: "session"  :: rest       -> cmd_session rest
  | _ :: "pipeline" :: path :: [] -> cmd_pipeline path
  | _ -> usage ()
