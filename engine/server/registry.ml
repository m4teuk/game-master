(* In-memory registry of active game sessions, keyed by game id.

   The reaper thread polls [has_any_connected] on each session; a game
   whose roster has been empty continuously for longer than the
   configured timeout is shut down and removed. *)

type entry = {
  game_id : string;
  session : Session.t;
  mutable empty_since : float option;
    (* [None] whenever any player is connected. [Some t] is the
       wall-clock time the session most recently became empty. *)
}

type t = {
  mutex : Mutex.t;
  table : (string, entry) Hashtbl.t;
}

let create () : t =
  { mutex = Mutex.create (); table = Hashtbl.create 16 }

let with_lock m f =
  Mutex.lock m;
  match f () with
  | x -> Mutex.unlock m; x
  | exception e -> Mutex.unlock m; raise e

let gen_id (reg : t) : string =
  (* Short human-typable IDs. Retry on collision — the chance is
     negligible but the cost of a second draw is free. *)
  let rec draw () =
    let s = Printf.sprintf "%08x" (Random.bits ()) in
    if Hashtbl.mem reg.table s then draw () else s
  in
  with_lock reg.mutex draw

let add (reg : t) (e : entry) : unit =
  with_lock reg.mutex (fun () -> Hashtbl.replace reg.table e.game_id e)

let find (reg : t) (id : string) : entry option =
  with_lock reg.mutex (fun () -> Hashtbl.find_opt reg.table id)

let remove (reg : t) (id : string) : unit =
  with_lock reg.mutex (fun () -> Hashtbl.remove reg.table id)

let ids (reg : t) : string list =
  with_lock reg.mutex (fun () ->
    Hashtbl.fold (fun k _ acc -> k :: acc) reg.table []
    |> List.sort String.compare)

(* Sweep: for each entry, if it has connections clear the timer;
   else either start it or, if past the timeout, collect for removal. *)
let reaper_step (reg : t) ~(timeout : float) : entry list =
  let now = Unix.gettimeofday () in
  let to_kill = ref [] in
  with_lock reg.mutex (fun () ->
    Hashtbl.iter (fun _id e ->
      if Session.has_any_connected e.session then
        e.empty_since <- None
      else
        match e.empty_since with
        | None -> e.empty_since <- Some now
        | Some t ->
          if now -. t >= timeout then to_kill := e :: !to_kill)
      reg.table);
  !to_kill

let start_reaper (reg : t) ~(timeout : float) : unit =
  let tick = max 1.0 (min 5.0 (timeout /. 4.0)) in
  let loop () =
    while true do
      (try Unix.sleepf tick with _ -> ());
      let victims = reaper_step reg ~timeout in
      List.iter (fun e ->
        Session.shutdown e.session
          ~reason:(Printf.sprintf
                     "no players connected for %.0fs" timeout);
        remove reg e.game_id)
        victims
    done
  in
  let _ : Thread.t = Thread.create loop () in
  ()
