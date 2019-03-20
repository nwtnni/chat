open Lwt.Infix

module AT = ANSITerminal

type rx = Lwt_io.input Lwt_io.channel
type tx = Lwt_io.output Lwt_io.channel

type client = {
  addr: Unix.sockaddr;
  name: string;
  color: AT.style;
}

(** Global list of connected clients. *)
let clients: (client * tx) list ref = ref []

(** Return a prettified string with all connected clients in alphabetical order. *)
let rec get_names () =
  !clients |> List.map fst
           |> List.sort (fun a b -> String.compare a.name b.name)
           |> List.map fmt_client_name
           |> List.fold_left (fun a b -> a ^ "-- - " ^ b ^ "\n") ""

(** Check whether a name is taken. *)
and exists_name name' =
  !clients |> List.map fst
           |> List.exists (fun client -> client.name = name')

(** Return the connected client with [addr], if they exist. *)
and get_client addr =
  List.find_opt (fun (client, _) -> client.addr = addr) !clients

(** Return all connected clients other than those with [addr]. *)
and get_others (addr: Unix.sockaddr) : (client * tx) list =
  List.filter (fun (client, _) -> client.addr <> addr) !clients

(** Insert the client into the connected list. *)
and insert_client (cl: client) (oc: tx) =
  clients := (cl, oc) :: !clients

(** Remove and return the client from the connected list. *)
and remove_client (addr: Unix.sockaddr): (client * tx) option =
  let client = get_client addr in
  let clients' = get_others addr in
  clients := clients';
  client

(** Continously accept new client connections. *)
and loop socket =
  Lwt_unix.accept socket >>= accept >>= fun () -> loop socket

(** Accept a new client connection and spawn a listening thread. *)
and accept (file, addr) =
  let ic = Lwt_io.of_fd Lwt_io.Input file in
  let oc = Lwt_io.of_fd Lwt_io.Output file in
  Lwt.async (fun () -> initialize addr ic oc);
  Lwt.return ()

(** Send a welcome message to the client and wait for name registration. *)
and initialize addr ic oc =
  "-- Welcome to lwt-chatroom! Please enter a nickname." 
  |> fmt_server
  |> Lwt_io.fprintl oc
  >>= fun () -> register addr ic oc

(** Register the client with the received name. *)
and register addr ic oc =
  Lwt_io.read_line_opt ic >>= function
  | None -> disconnect addr
  | Some name when exists_name name ->
    Printf.sprintf "-- Error: %s is already taken." name
    |> fmt_server
    |> Lwt_io.fprintl oc
    >>= fun () -> register addr ic oc
  | Some name ->
    connect addr name oc
    >>= fun () -> listen addr ic

(** Continuously listen for new commands on the channel. *)
and listen addr ic =
  Lwt_io.read_line_opt ic >>= function
  | None -> disconnect addr
  | Some command -> parse addr command >>= fun () -> listen addr ic

(** Parse and execute [command]. *)
and parse addr command =
  let ws = Str.regexp "[ ]+" in
  match Str.bounded_split ws command 2 with
  | ["q"]        | ["quit"]         -> disconnect addr
  | ["h"]        | ["help"]         -> help addr
  | ["l"]        | ["list"]         -> list_names addr
  | ["n"; name]  | ["nick"; name]   -> change_name addr name
  | ["c"; color] | ["color"; color] -> change_color addr color
  | _ -> broadcast_client addr command

(** Insert the client into the connected list and notify the room. *)
and connect addr name oc =
  insert_client { addr; name; color = AT.default } oc;
  let join = Printf.sprintf "%s has joined the chat." name in
  broadcast_server join

(** Remove the client from the connected list and notify the room. *)
and disconnect addr =
  remove_client addr >> fun (client, _) ->
  Printf.sprintf "%s has left the chat." client.name
  |> broadcast_server

(** Send the client a help message. *)
and help addr =
  get_client addr >> fun (_, oc) ->
  Lwt_io.fprintf oc
    "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n"
    "-----------------------------------------------"
    (fmt_server "-- Welcome to lwt-chatroom! Commands are below.")
    "-----------------------------------------------"
    "-- | [q]uit        : Exit chatroom"
    "-- | [h]elp        : Display commands"
    "-- | [l]ist        : List connected clients"
    "-- | [n]ick name   : Change name to [name]"
    "-- | [c]olor color : Change color to [color]"
    "-----------------------------------------------"
    (Printf.sprintf "-- Where color is one of\n-- - %s\n-- - %s\n-- - %s\n-- - %s\n-- - %s\n-- - %s"
      (AT.sprintf [AT.Bold; AT.green] "green")
      (AT.sprintf [AT.Bold; AT.yellow] "yellow")
      (AT.sprintf [AT.Bold; AT.blue] "blue")
      (AT.sprintf [AT.Bold; AT.magenta] "magenta")
      (AT.sprintf [AT.Bold; AT.cyan] "cyan")
      (AT.sprintf [AT.Bold; AT.white] "white"))

and list_names addr =
  get_client addr >> fun (_, oc) ->
  Lwt_io.fprintf oc
    "%s\n%s\n%s\n%s"
    ("------------")
    (fmt_server "-- Connected")
    ("------------")
    (get_names ())

(** Change the client's nickname and notify the room. *)
and change_name addr name' =
  remove_client addr >> fun (client, oc) ->
  if exists_name name' then begin
    insert_client client oc;
    Printf.sprintf "-- Error: %s is already taken." name'
    |> fmt_server
    |> Lwt_io.fprintl oc
  end else begin
    insert_client { client with name = name' } oc;
    Printf.sprintf "%s has changed their name to %s." client.name name'
    |> broadcast_server
  end

(** Change the client's color and notify the room. *)
and change_color addr color =
  remove_client addr >> fun (client, oc) ->
  let color' = parse_color client.color color in
  insert_client { client with color = color' } oc;
  Printf.sprintf
    "%s %s %s."
    (fmt_client_name client)
    (fmt_server "has changed their")
    (AT.sprintf [AT.Bold; color'] "color")
  |> broadcast_server

(** Attempt to parse a color from unknown string input, using [default] as a fallback. *)
and parse_color default = function
| "green" -> AT.green
| "yellow" -> AT.yellow
| "blue" -> AT.blue
| "magenta" -> AT.magenta
| "cyan" -> AT.cyan
| "white" -> AT.white
| _ -> default

(** Monadic plumbing for disconnected clients. *)
and (>>) (opt: (client * tx) option) (f: (client * tx) -> unit Lwt.t) : unit Lwt.t =
  match opt with 
  | None -> Lwt.return ()
  | Some client -> f client

(** Broadcast a message from the server. *)
and broadcast_server message =
  message |> fmt_server
          |> fmt_time
          |> broadcast

(** Broadcast a message from a client. *)
and broadcast_client addr message =
  match get_client addr with
  | None -> Lwt.return ()
  | Some (client, _) ->
    message |> fmt_client client
            |> fmt_time
            |> broadcast

(** Broadcast a message to all connected clients. *)
and broadcast message =
  !clients |> List.map snd
           |> List.map (Lwt_io.fprintl)
           |> List.map (fun write -> write message)
           |> Lwt.join

(** Recolor server announcements. *)
and fmt_server message =
  (AT.sprintf [AT.Bold; AT.red] "%s" message)

(** Color this client's name. *)
and fmt_client_name client =
  AT.sprintf [AT.Bold; client.color] "%s" client.name

(** Prepend the client's colored name to [message]. *)
and fmt_client client message =
  Printf.sprintf 
    "%s: %s"
    (fmt_client_name client)
    message

(** Prepend the current time to [message]. *)
and fmt_time message =
  let time = Unix.gettimeofday () |> Unix.localtime in
  Printf.sprintf
    "[%02i/%02i/%02i %02i:%02i:%02i] %s"
    (time.tm_mon + 1)
    time.tm_mday
    (time.tm_year mod 100)
    time.tm_hour
    time.tm_min
    time.tm_sec
    message

(** Main entrypoint. Bind socket to server and loop over new connections. *)
let () =
  let port = 10000 in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, port) in
  Lwt_main.run begin
    Lwt_unix.bind sock addr
    >|= (fun () -> Lwt_unix.listen sock 100)
    >>= (fun () -> loop sock)
  end
