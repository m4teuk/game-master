(* Per-game state machine. One driver thread + N reader threads.

   Layout:
     - [slots] is roster order; each slot holds at most one live conn.
     - [queue] is FIFO of {player, input} lines pushed by reader threads.
     - [mutex]/[cond] gate the queue and the state refs.
     - Driver thread blocks on [cond], drains [queue] under [mutex],
       then releases and runs engine calls without holding the mutex
       (engine calls can be slow and must not block readers). *)

type slot = {
  player : Engine.player;
  mutable conn : Protocol.conn option;
}

type queued = {
  q_player : Engine.player;
  q_input : string;
  q_conn : Protocol.conn;
    (* The conn that produced this input. Used to detect "this line is
       stale because the player has since reconnected on a different
       socket" — in that case we drop it. *)
}

type t = {
  parsed : Engine.parsed;
  mutable state : Engine.engine_state;
  slots : slot list;
  mutex : Mutex.t;
  cond : Condition.t;
  queue : queued Queue.t;
  mutable started : bool;
  mutable ended : bool;
  mutable connected_count : int;
  on_game_end : unit -> unit;
}

let with_lock m f =
  Mutex.lock m;
  match f () with
  | x -> Mutex.unlock m; x
  | exception e -> Mutex.unlock m; raise e

let connected_conns_locked t =
  List.filter_map (fun s -> match s.conn with
    | Some c -> Some (s.player, c)
    | None -> None) t.slots

let snapshot_conns t =
  with_lock t.mutex (fun () -> connected_conns_locked t)

let _broadcast t msg =
  List.iter (fun (_, c) -> Protocol.write_block c msg) (snapshot_conns t)

let broadcast_line t msg =
  List.iter (fun (_, c) -> Protocol.write_line c msg) (snapshot_conns t)

let display_all t =
  let es = with_lock t.mutex (fun () -> t.state) in
  List.iter (fun (p, c) ->
    let rendered = Engine.display t.parsed es ~player:p in
    Protocol.write_block c (Printf.sprintf "--- your view ---\n%s" rendered))
    (snapshot_conns t)

let slot_of t player =
  List.find (fun s -> s.player = player) t.slots

(* Set ended, flush everything, close conns, fire callback. Idempotent. *)
let finalise t ~reason_msgs =
  let conns_to_close =
    with_lock t.mutex (fun () ->
      if t.ended then []
      else begin
        t.ended <- true;
        let cs = connected_conns_locked t in
        List.iter (fun s -> s.conn <- None) t.slots;
        t.connected_count <- 0;
        Condition.broadcast t.cond;
        cs
      end)
  in
  match conns_to_close with
  | [] -> ()
  | _ ->
    (* Send personalised tail messages first, then close. *)
    List.iter (fun (p, c) ->
      (match List.assoc_opt p reason_msgs with
       | Some msg -> Protocol.write_block c msg
       | None -> ());
      Protocol.close c)
      conns_to_close;
    t.on_game_end ()

let shutdown t ~reason =
  let conns = snapshot_conns t in
  let msgs = List.map (fun (p, _) ->
    (p, Printf.sprintf "=== Session ended: %s ===\n" reason)) conns in
  finalise t ~reason_msgs:msgs

(* Broadcast personalised terminal outcome, then tear down. *)
let end_normally t =
  let parsed = t.parsed in
  let es = with_lock t.mutex (fun () -> t.state) in
  let msgs =
    List.map (fun s ->
      let msg =
        match Engine.status parsed es ~player:s.player with
        | Engine.Ended m ->
          Printf.sprintf "=== Game over ===\n%s\n" m
        | Engine.Ongoing ->
          "=== Game over ===\n"
      in
      (s.player, msg))
      t.slots
  in
  finalise t ~reason_msgs:msgs

(* Called after a successful [apply]. Returns true if the game ended. *)
let check_terminal t : bool =
  let es = with_lock t.mutex (fun () -> t.state) in
  match t.slots with
  | [] -> false
  | s :: _ ->
    (match Engine.status t.parsed es ~player:s.player with
     | Engine.Ongoing -> false
     | Engine.Ended _ -> end_normally t; true)

let handle_action t q =
  match Engine.apply t.parsed t.state ~player:q.q_player ~input:q.q_input with
  | Error (Engine.Invalid msg) ->
    (* Targeted reply; only meaningful if this conn is still current. *)
    let still_current =
      with_lock t.mutex (fun () ->
        match (slot_of t q.q_player).conn with
        | Some c when c == q.q_conn -> true
        | _ -> false)
    in
    if still_current then
      Protocol.write_line q.q_conn (Printf.sprintf "error: %s" msg)
  | Error (Engine.Fatal msg) ->
    broadcast_line t (Printf.sprintf "fatal engine error: %s" msg);
    let tag_all = List.map (fun s ->
      (s.player, "=== Session terminated by engine fatal ===\n")) t.slots in
    finalise t ~reason_msgs:tag_all
  | Ok (new_es, rendered) ->
    with_lock t.mutex (fun () -> t.state <- new_es);
    (* Broadcast the rendered action (spec requires it be self-describing). *)
    broadcast_line t rendered;
    display_all t;
    let _ended = check_terminal t in
    ()

let handle_empty t q =
  (* Empty line → redisplay the acting player's view, only to them. *)
  let (es, still_current) =
    with_lock t.mutex (fun () ->
      let cur =
        match (slot_of t q.q_player).conn with
        | Some c when c == q.q_conn -> true
        | _ -> false
      in
      (t.state, cur))
  in
  if still_current then
    let rendered = Engine.display t.parsed es ~player:q.q_player in
    Protocol.write_block q.q_conn
      (Printf.sprintf "--- your view ---\n%s" rendered)

let driver_loop t =
  let rec loop () =
    let next =
      Mutex.lock t.mutex;
      while (not t.ended) && Queue.is_empty t.queue do
        Condition.wait t.cond t.mutex
      done;
      if t.ended then begin Mutex.unlock t.mutex; None end
      else begin
        let q = Queue.pop t.queue in
        Mutex.unlock t.mutex;
        Some q
      end
    in
    match next with
    | None -> ()
    | Some q ->
      if String.trim q.q_input = "" then handle_empty t q
      else handle_action t q;
      loop ()
  in
  try loop ()
  with e ->
    (* Unexpected — log and die, but don't leak the lock. *)
    (try Mutex.unlock t.mutex with _ -> ());
    prerr_endline ("driver thread crashed: " ^ Printexc.to_string e);
    let tag_all =
      List.map (fun s ->
        (s.player, "=== Server internal error ===\n"))
        t.slots
    in
    finalise t ~reason_msgs:tag_all

let reader_loop t player conn =
  let rec loop () =
    match Protocol.read_line conn with
    | None -> ()    (* EOF — client dropped *)
    | Some line ->
      let still_current =
        with_lock t.mutex (fun () ->
          if t.ended then false
          else match (slot_of t player).conn with
            | Some c when c == conn ->
              Queue.add { q_player = player; q_input = line; q_conn = conn }
                t.queue;
              Condition.signal t.cond;
              true
            | _ -> false)
      in
      if still_current then loop ()
  in
  loop ();
  (* EOF or we've been displaced. If we're still the current conn for
     this slot, clear it; never touch another reader's conn. *)
  let became_empty =
    with_lock t.mutex (fun () ->
      match (slot_of t player).conn with
      | Some c when c == conn ->
        (slot_of t player).conn <- None;
        t.connected_count <- t.connected_count - 1;
        Condition.broadcast t.cond;
        t.connected_count = 0
      | _ -> false)
  in
  ignore became_empty;
  Protocol.close conn

let create ~parsed ~initial ~on_game_end =
  let slots =
    List.map (fun p -> { player = p; conn = None })
      (Engine.players initial)
  in
  let t = {
    parsed;
    state = initial;
    slots;
    mutex = Mutex.create ();
    cond = Condition.create ();
    queue = Queue.create ();
    started = false;
    ended = false;
    connected_count = 0;
    on_game_end;
  } in
  let _ : Thread.t = Thread.create driver_loop t in
  t

let has_any_connected t =
  with_lock t.mutex (fun () -> t.connected_count > 0)

type attach_effect =
  | E_rejected of string
  | E_ended
  | E_waiting of { displaced : Protocol.conn option; remaining : int;
                   announce_to : (Engine.player * Protocol.conn) list }
  | E_starting of { displaced : Protocol.conn option }
  | E_reconnect of { displaced : Protocol.conn option }

let attach t player conn =
  let eff =
    with_lock t.mutex (fun () ->
      if t.ended then E_ended
      else match List.find_opt (fun s -> s.player = player) t.slots with
        | None ->
          E_rejected (Printf.sprintf
                        "'%s' is not in the roster" player)
        | Some slot ->
          let displaced = slot.conn in
          slot.conn <- Some conn;
          (match displaced with
           | None -> t.connected_count <- t.connected_count + 1
           | Some _ -> ());
          Condition.broadcast t.cond;
          let total = List.length t.slots in
          let all_joined = t.connected_count = total in
          if t.started then
            E_reconnect { displaced }
          else if all_joined then begin
            t.started <- true;
            E_starting { displaced }
          end else begin
            let others =
              List.filter_map (fun s ->
                if s.player = player then None
                else Option.map (fun c -> (s.player, c)) s.conn)
                t.slots
            in
            E_waiting { displaced; remaining = total - t.connected_count;
                        announce_to = others }
          end)
  in
  match eff with
  | E_ended ->
    Protocol.write_line conn "Game has already ended.";
    Protocol.close conn;
    Error "game ended"
  | E_rejected msg ->
    Error msg
  | E_waiting { displaced; remaining; announce_to } ->
    (match displaced with
     | Some d ->
       Protocol.write_line d "Replaced by a new connection.";
       Protocol.close d
     | None -> ());
    Protocol.write_line conn
      (Printf.sprintf "Joined as '%s'. Waiting for %d more player(s)."
         player remaining);
    List.iter (fun (_, c) ->
      Protocol.write_line c (Printf.sprintf "%s joined." player))
      announce_to;
    let _ : Thread.t = Thread.create (fun () -> reader_loop t player conn) () in
    Ok ()
  | E_starting { displaced } ->
    (match displaced with
     | Some d ->
       Protocol.write_line d "Replaced by a new connection.";
       Protocol.close d
     | None -> ());
    Protocol.write_line conn (Printf.sprintf "Joined as '%s'." player);
    broadcast_line t "=== Game starting ===";
    display_all t;
    let _ : Thread.t = Thread.create (fun () -> reader_loop t player conn) () in
    Ok ()
  | E_reconnect { displaced } ->
    (match displaced with
     | Some d ->
       Protocol.write_line d "Replaced by a new connection.";
       Protocol.close d
     | None -> ());
    let es = with_lock t.mutex (fun () -> t.state) in
    let rendered = Engine.display t.parsed es ~player in
    Protocol.write_block conn
      (Printf.sprintf "Reconnected as '%s'.\n--- your view ---\n%s"
         player rendered);
    let _ : Thread.t = Thread.create (fun () -> reader_loop t player conn) () in
    Ok ()
