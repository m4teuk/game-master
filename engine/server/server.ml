(* Multi-game server.

   Each connection starts in a per-thread lobby that drives the user
   through one of two flows:
     - join an existing game by id
     - create a new one (prebuilt by name, or upload custom source
       terminated by a line "EOF")

   Once the connection is handed to a session it is driven by
   [Session]; the lobby thread exits.

   A reaper thread in [Registry] shuts down games with no connected
   players for longer than the configured timeout. *)

(* ---------------------------------------------------------------- *)
(* Lobby I/O primitives                                               *)
(* ---------------------------------------------------------------- *)

let prompt conn msg : string option =
  Protocol.write_line conn msg;
  Option.map String.trim (Protocol.read_line conn)

let say = Protocol.write_line
let say_block = Protocol.write_block

(* ---------------------------------------------------------------- *)
(* Prebuilt ruleset folder                                            *)
(* ---------------------------------------------------------------- *)

type prebuilt = { display : string; path : string }

let list_prebuilts (folder : string) : prebuilt list =
  try
    Sys.readdir folder
    |> Array.to_list
    |> List.filter (fun n -> Filename.check_suffix n ".game")
    |> List.map (fun n -> {
        display = Filename.chop_suffix n ".game";
        path = Filename.concat folder n;
      })
    |> List.sort (fun a b -> String.compare a.display b.display)
  with Sys_error _ -> []

let pick_prebuilt conn folder : prebuilt option =
  let games = list_prebuilts folder in
  if games = [] then begin
    say conn "No prebuilt games available.";
    None
  end else begin
    say conn "Available games:";
    List.iteri (fun i g ->
      say conn (Printf.sprintf "  %d. %s" (i + 1) g.display)) games;
    let rec ask () =
      match prompt conn "Pick a game (number or name):" with
      | None -> None
      | Some raw ->
        let by_num =
          match int_of_string_opt raw with
          | Some i when i >= 1 && i <= List.length games ->
            Some (List.nth games (i - 1))
          | _ -> None
        in
        match by_num with
        | Some g -> Some g
        | None ->
          match List.find_opt (fun g -> g.display = raw) games with
          | Some g -> Some g
          | None -> say conn "Not a valid choice."; ask ()
    in
    ask ()
  end

(* ---------------------------------------------------------------- *)
(* Custom code upload                                                 *)
(* ---------------------------------------------------------------- *)

let read_custom_source conn : string option =
  say conn "Paste your .game source.";
  say conn "End with a line containing only 'EOF'.";
  let buf = Buffer.create 4096 in
  let rec loop () =
    match Protocol.read_line conn with
    | None -> None
    | Some line ->
      if String.trim line = "EOF" then Some (Buffer.contents buf)
      else begin
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
        loop ()
      end
  in
  loop ()

(* ---------------------------------------------------------------- *)
(* Interactive options / players / seed                               *)
(* ---------------------------------------------------------------- *)

let prompt_one_option conn (f : Engine.option_field)
  : Engine.option_value option =
  let type_str = match f.ty with
    | Engine.OT_num -> "Num"
    | OT_text -> "Text"
    | OT_enum variants -> "one of " ^ String.concat " | " variants
  in
  let default_str = match f.default with
    | Engine.OV_num n -> string_of_int n
    | OV_text s -> Printf.sprintf "%S" s
    | OV_enum c -> c
  in
  let rec ask () =
    let msg = Printf.sprintf "  %s (%s) [default %s]:"
                f.name type_str default_str in
    match prompt conn msg with
    | None -> None
    | Some "" -> Some f.default
    | Some raw ->
      (match f.ty with
       | OT_num ->
         (match int_of_string_opt raw with
          | Some n -> Some (Engine.OV_num n)
          | None -> say conn "  not a number, try again"; ask ())
       | OT_text -> Some (Engine.OV_text raw)
       | OT_enum variants ->
         if List.mem raw variants then Some (Engine.OV_enum raw)
         else begin
           say conn (Printf.sprintf "  must be one of {%s}, try again"
                       (String.concat " | " variants));
           ask ()
         end)
  in
  ask ()

let prompt_options conn (schema : Engine.option_field list)
  : (string * Engine.option_value) list option =
  if schema = [] then Some []
  else begin
    say conn "Options:";
    let rec collect acc = function
      | [] -> Some (List.rev acc)
      | f :: rest ->
        match prompt_one_option conn f with
        | None -> None
        | Some v -> collect ((f.name, v) :: acc) rest
    in
    collect [] schema
  end

let prompt_players conn (creator : string)
  : string list option =
  let rec ask () =
    let msg = Printf.sprintf
                "Enter players, comma-separated, including yourself ('%s'):"
                creator
    in
    match prompt conn msg with
    | None -> None
    | Some raw ->
      let ps = Common.parse_players_csv raw in
      if not (List.mem creator ps) then begin
        say conn (Printf.sprintf "Must include your own id '%s'." creator);
        ask ()
      end else if List.length (List.sort_uniq String.compare ps)
                 <> List.length ps then begin
        say conn "Duplicate player ids not allowed."; ask ()
      end else Some ps
  in
  ask ()

let prompt_seed conn : bytes option =
  match prompt conn "Seed (32 hex chars, or blank for random):" with
  | None -> None
  | Some "" -> Some (Common.random_seed ())
  | Some raw ->
    try Some (Common.parse_seed_hex raw)
    with Failure msg -> say conn ("  " ^ msg ^ " — using random"); Some (Common.random_seed ())

(* ---------------------------------------------------------------- *)
(* Join an existing game                                              *)
(* ---------------------------------------------------------------- *)

let handle_join (conn : Protocol.conn) (entry : Registry.entry) : unit =
  match prompt conn "Enter your playerId:" with
  | None -> Protocol.close conn
  | Some pid ->
    match Session.attach entry.session pid conn with
    | Ok () -> ()  (* session owns the conn now *)
    | Error msg ->
      say conn ("Cannot join: " ^ msg);
      Protocol.close conn

(* ---------------------------------------------------------------- *)
(* Create a new game                                                  *)
(* ---------------------------------------------------------------- *)

(* Returns Ok (creator, parsed) or Error to restart, or None to quit. *)
type source_outcome =
  | So_parsed of string * Engine.parsed  (* (display_name, parsed) *)
  | So_retry
  | So_quit

let obtain_parsed_source conn folder : source_outcome =
  let rec pick () =
    match prompt conn "Prebuilt or custom? (prebuilt/custom):" with
    | None -> So_quit
    | Some s ->
      match String.lowercase_ascii s with
      | "prebuilt" | "p" ->
        (match pick_prebuilt conn folder with
         | None -> So_quit
         | Some g ->
           let src =
             try Common.read_file g.path
             with Sys_error m ->
               say conn ("Cannot read file: " ^ m); ""
           in
           if src = "" then So_retry
           else match Common.load_parsed ~source_name:g.path src with
             | Ok p -> So_parsed (g.display, p)
             | Error msgs ->
               say conn "Prebuilt failed to load (bug in server state):";
               List.iter (fun m -> say conn ("  " ^ m)) msgs;
               So_retry)
      | "custom" | "c" ->
        (match read_custom_source conn with
         | None -> So_quit
         | Some src ->
           (match Common.load_parsed ~source_name:"<custom>" src with
            | Ok p -> So_parsed ("<custom>", p)
            | Error msgs ->
              say conn "Failed to compile:";
              List.iter (fun m -> say conn ("  " ^ m)) msgs;
              So_retry))
      | _ ->
        say conn "Please answer 'prebuilt' or 'custom'."; pick ()
  in
  pick ()

let handle_create
    ~(folder : string)
    ~(registry : Registry.t)
    ~(timeout : float)
    (conn : Protocol.conn)
    (creator : string)
  : unit =
  let rec loop () =
    match obtain_parsed_source conn folder with
    | So_quit -> Protocol.close conn
    | So_retry -> loop ()
    | So_parsed (display, parsed) ->
      (match prompt_options conn (Engine.options_form parsed) with
       | None -> Protocol.close conn
       | Some options ->
         match prompt_players conn creator with
         | None -> Protocol.close conn
         | Some players ->
           match prompt_seed conn with
           | None -> Protocol.close conn
           | Some seed ->
             (match Engine.init_state parsed ~options ~players ~seed with
              | Error err ->
                say conn ("Setup failed: " ^ Common.setup_error_to_string err);
                loop ()
              | Ok engine_state ->
                let game_id = Registry.gen_id registry in
                let entry_ref = ref None in
                let on_game_end () =
                  match !entry_ref with
                  | Some (e : Registry.entry) ->
                    Registry.remove registry e.game_id
                  | None -> ()
                in
                let session =
                  Session.create ~parsed ~initial:engine_state ~on_game_end
                in
                let entry = {
                  Registry.game_id;
                  session;
                  empty_since = None;
                } in
                entry_ref := Some entry;
                Registry.add registry entry;
                say conn (Printf.sprintf
                            "Created game '%s' (ruleset: %s)." game_id display);
                say conn (Printf.sprintf
                            "If no player is connected for more than %.0fs, \
                             the game will end."
                            timeout);
                (match Session.attach session creator conn with
                 | Ok () -> ()
                 | Error msg ->
                   say conn ("Failed to join your own game: " ^ msg);
                   Registry.remove registry game_id;
                   Session.shutdown session ~reason:"creator could not join";
                   Protocol.close conn)))
  in
  loop ()

(* ---------------------------------------------------------------- *)
(* Top-level per-connection flow                                      *)
(* ---------------------------------------------------------------- *)

let handle_client
    ~(folder : string)
    ~(registry : Registry.t)
    ~(timeout : float)
    (conn : Protocol.conn)
  : unit =
  let greeting =
    "Welcome.\n\
     To join an existing game, enter its game ID.\n\
     To create a new one, type 'new'."
  in
  say_block conn greeting;
  match prompt conn "game id or 'new':" with
  | None -> Protocol.close conn
  | Some "" -> say conn "Empty input; disconnecting."; Protocol.close conn
  | Some input ->
    if String.lowercase_ascii input = "new" then begin
      match prompt conn "Choose your playerId:" with
      | None -> Protocol.close conn
      | Some creator ->
        if String.trim creator = "" then begin
          say conn "Empty playerId; disconnecting."; Protocol.close conn
        end else handle_create ~folder ~registry ~timeout conn creator
    end else begin
      match Registry.find registry input with
      | None ->
        say conn (Printf.sprintf "No such game: '%s'." input);
        Protocol.close conn
      | Some entry -> handle_join conn entry
    end

(* ---------------------------------------------------------------- *)
(* Main                                                               *)
(* ---------------------------------------------------------------- *)

let usage () =
  prerr_endline
    "Usage: server [-a ADDR] [-p PORT] [-t TIMEOUT_SECS] [-f PREBUILT_FOLDER]";
  exit 2

let () =
  let addr = ref "0.0.0.0" in
  let port = ref 3301 in
  let timeout = ref 300 in
  let folder = ref "../game-examples" in
  let speclist = [
    "-a", Arg.Set_string addr, " address to bind (default 0.0.0.0)";
    "-p", Arg.Set_int port, " port (default 3301)";
    "-t", Arg.Set_int timeout, " connection timeout seconds (default 300)";
    "-f", Arg.Set_string folder,
          " folder with prebuilt .game files (default ../game-examples)";
  ] in
  Arg.parse speclist (fun _ -> usage ()) "server";
  if !timeout <= 0 || !folder = "" then usage ();

  Random.self_init ();

  (* Writes to a peer that has RSTed otherwise raise SIGPIPE and kill
     the whole process, tearing down every other live game with it. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  let registry = Registry.create () in
  Registry.start_reaper registry ~timeout:(float_of_int !timeout);

  (* Graceful shutdown on SIGTERM/SIGINT: try to tell every live session
     we're going away, then exit unconditionally. Any failure along the
     way must not prevent termination — systemd expects us to die. *)
  let graceful_exit signame _ =
    (try
       Printf.eprintf "received %s, shutting down\n%!" signame;
       List.iter (fun id ->
         try
           match Registry.find registry id with
           | Some e -> Session.shutdown e.session ~reason:"server shutting down"
           | None -> ()
         with _ -> ())
         (Registry.ids registry)
     with _ -> ());
    exit 0
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (graceful_exit "SIGTERM"));
  Sys.set_signal Sys.sigint  (Sys.Signal_handle (graceful_exit "SIGINT"));

  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.setsockopt sock SO_REUSEADDR true;
  let bind_addr =
    try Common.resolve_address !addr
    with Failure msg -> prerr_endline msg; exit 1
  in
  (try
     Unix.bind sock (Unix.ADDR_INET (bind_addr, !port))
   with Unix.Unix_error (e, _, _) ->
     prerr_endline ("bind failed: " ^ Unix.error_message e); exit 1);
  Unix.listen sock 32;
  Printf.printf
    "server listening on %s:%d (timeout=%ds, prebuilt folder=%s)\n%!"
    !addr !port !timeout !folder;

  while true do
    match Unix.accept sock with
    | exception Unix.Unix_error _ -> ()
    | (fd, addr_of) ->
      let conn = Protocol.make fd addr_of in
      let _ : Thread.t =
        Thread.create (fun () ->
          try handle_client ~folder:!folder ~registry
                ~timeout:(float_of_int !timeout) conn
          with e ->
            prerr_endline
              ("lobby thread crashed: " ^ Printexc.to_string e);
            Protocol.close conn)
          ()
      in ()
  done
