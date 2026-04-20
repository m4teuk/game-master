(* Single-game server. Fixed roster + options at launch. Runs until
   ^C. On game end, every client is disconnected and the listening
   socket is closed — the process stays alive until signalled. *)

let usage () =
  prerr_endline
    "Usage: simple_server -a ADDR -p PORT -f FILE -P PLAYERS \
     [-o OPTS] [-s SEED_HEX32]";
  prerr_endline "  OPTS  : 'k=v,k=v' pairs matching the game's options block";
  prerr_endline "  SEED  : 32 hex chars (16 bytes); random if omitted";
  exit 2

let () =
  let addr = ref "0.0.0.0" in
  let port = ref 0 in
  let file = ref "" in
  let players_arg = ref "" in
  let opts_arg = ref "" in
  let seed_arg = ref "" in
  let speclist = [
    "-a", Arg.Set_string addr, " address to bind (default 0.0.0.0)";
    "-p", Arg.Set_int port, " port";
    "-f", Arg.Set_string file, " .game file";
    "-P", Arg.Set_string players_arg, " player CSV, e.g. alice,bob";
    "-o", Arg.Set_string opts_arg, " options CSV, k=v,k=v";
    "-s", Arg.Set_string seed_arg, " seed (32 hex chars)";
  ] in
  Arg.parse speclist (fun _ -> usage ()) "simple_server";
  if !file = "" || !players_arg = "" || !port = 0 then usage ();

  (* Load ruleset. *)
  let src =
    try Common.read_file !file
    with Sys_error msg ->
      prerr_endline ("cannot read file: " ^ msg); exit 1
  in
  let parsed =
    match Common.load_parsed ~source_name:!file src with
    | Ok p -> p
    | Error msgs ->
      prerr_endline "Failed to load ruleset:";
      List.iter (fun m -> prerr_endline ("  " ^ m)) msgs;
      exit 1
  in

  let players = Common.parse_players_csv !players_arg in
  if players = [] then (prerr_endline "empty player list"; exit 2);

  let options =
    match Common.parse_options_csv (Engine.options_form parsed) !opts_arg with
    | Ok o -> o
    | Error msg -> prerr_endline ("bad -o: " ^ msg); exit 2
  in

  let seed =
    try
      if !seed_arg = "" then Common.random_seed ()
      else Common.parse_seed_hex !seed_arg
    with Failure msg -> prerr_endline ("bad -s: " ^ msg); exit 2
  in

  let engine_state =
    match Engine.init_state parsed ~options ~players ~seed with
    | Ok s -> s
    | Error err ->
      prerr_endline (Common.setup_error_to_string err); exit 1
  in

  (* Listener socket. *)
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
  Unix.listen sock 8;
  Printf.printf
    "simple_server listening on %s:%d for players: %s\n%!"
    !addr !port (String.concat ", " players);

  (* End-of-game signal: driver calls on_game_end, main thread parks. *)
  let end_mutex = Mutex.create () in
  let end_cond = Condition.create () in
  let ended_flag = ref false in

  let on_game_end () =
    Mutex.lock end_mutex;
    ended_flag := true;
    Condition.broadcast end_cond;
    Mutex.unlock end_mutex;
    (* Close listener so no further connects are accepted. *)
    (try Unix.close sock with _ -> ());
    Printf.printf
      "Game ended. No new connections accepted; ^C to exit.\n%!"
  in

  let session = Session.create ~parsed ~initial:engine_state ~on_game_end in

  let accept_one fd addr =
    let conn = Protocol.make fd addr in
    let _ : Thread.t = Thread.create (fun () ->
      Protocol.write_line conn
        (Printf.sprintf
           "Welcome. Expected players: %s.\nEnter your playerId:"
           (String.concat ", " players));
      match Protocol.read_line conn with
      | None -> Protocol.close conn
      | Some raw ->
        let pid = String.trim raw in
        if not (List.mem pid players) then begin
          Protocol.write_line conn
            (Printf.sprintf
               "'%s' is not a valid playerId. Expected one of: %s"
               pid (String.concat ", " players));
          Protocol.close conn
        end else
          (match Session.attach session pid conn with
           | Ok () -> ()
           | Error msg ->
             Protocol.write_line conn msg;
             Protocol.close conn)
    ) () in
    ()
  in

  let accept_loop () =
    let keep_going = ref true in
    while !keep_going do
      match Unix.accept sock with
      | (fd, addr) -> accept_one fd addr
      | exception Unix.Unix_error _ -> keep_going := false
    done
  in
  let _ : Thread.t = Thread.create accept_loop () in

  (* Park until the session signals end, then park forever awaiting ^C. *)
  Mutex.lock end_mutex;
  while not !ended_flag do
    Condition.wait end_cond end_mutex
  done;
  Mutex.unlock end_mutex;
  while true do Unix.sleep 3600 done
