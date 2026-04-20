(* Line-based socket I/O. Each conn wraps a TCP fd with a read channel,
   a write channel, and a write mutex so multiple threads can broadcast
   into it without interleaving bytes. *)

type conn = {
  fd : Unix.file_descr;
  ic : in_channel;
  oc : out_channel;
  write_mutex : Mutex.t;
  peer : string;
  mutable closed : bool;
}

let make fd sockaddr =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  let peer = match sockaddr with
    | Unix.ADDR_INET (a, p) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr a) p
    | Unix.ADDR_UNIX s -> s
  in
  { fd; ic; oc; write_mutex = Mutex.create (); peer; closed = false }

let peer c = c.peer

let write_raw c s =
  Mutex.lock c.write_mutex;
  (try
     if not c.closed then begin
       output_string c.oc s;
       flush c.oc
     end
   with _ -> ());
  Mutex.unlock c.write_mutex

let write_line c s =
  write_raw c (s ^ "\n")

let write_block c s =
  let ends_nl =
    String.length s > 0 && s.[String.length s - 1] = '\n'
  in
  write_raw c (if ends_nl then s else s ^ "\n")

let read_line c =
  try Some (input_line c.ic)
  with End_of_file | Sys_error _ -> None

let close c =
  Mutex.lock c.write_mutex;
  if not c.closed then begin
    c.closed <- true;
    (try flush c.oc with _ -> ());
    (try Unix.shutdown c.fd Unix.SHUTDOWN_ALL with _ -> ());
    (try Unix.close c.fd with _ -> ())
  end;
  Mutex.unlock c.write_mutex

let is_closed c = c.closed
