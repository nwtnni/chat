open Lwt.Infix
open Chat

module AT = ANSITerminal

(** Main entry point *)
let rec main () =
  let port = 10000 in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, port) in
  Lwt_main.run begin
    Lwt_unix.bind sock addr
    >|= (fun () -> Lwt_unix.listen sock 100)
    >>= (fun () -> loop sock)
  end

(** Continously accept new client connections. *)
and loop socket =
  Lwt_unix.accept socket >>= accept >>= fun () -> loop socket

(** Accept a new client connection and spawn a listening thread. *)
and accept (file, addr) =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input file in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output file in
  Lwt.async begin fun () -> Lwt.catch
    (fun () -> initialize addr ic oc)
    (fun _ -> disconnect addr)
  end;
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
  | None -> Lwt.fail_with "Disconnected"
  | Some name when Connected.mem name ->
    Printf.sprintf "-- Error: %s is already taken." name
    |> fmt_server
    |> Lwt_io.fprintl oc
    >>= fun () -> register addr ic oc
  | Some name ->
    connect addr name oc
    >>= fun () -> help name
    >>= fun () -> listen name ic

(** Continuously listen for new commands on the channel. *)
and listen name ic =
  Lwt_io.read_line_opt ic >>= function
  | None -> Lwt.fail_with "Disconnected"
  | Some command -> parse name command >>= fun name' -> listen name' ic

(** Parse and execute [command]. *)
and parse name command =
  let ws = Str.regexp "[ ]+" in
  match Str.bounded_split ws command 3 with
  | ["q"] | ["quit"] ->
    Lwt.fail_with "Disconnected"
  | ["h"] | ["help"] ->
    help name >|= fun () -> name
  | ["l"] | ["list"] ->
    list_names name >|= fun () -> name
  | ["n"; name'] | ["nick"; name'] ->
    change_name name name' >|= fun () -> name'
  | ["c"; color] | ["color"; color] ->
    change_color name color >|= fun () -> name
  | ["w"; name'; m] | ["whisper"; name'; m] ->
    whisper name name' m >|= fun () -> name
  | _ ->
    broadcast_client name command >|= fun () -> name

(** Insert the client into the connected list and notify the room. *)
and connect addr name oc =
  let client = Client.make addr name in
  Connected.insert client oc;
  let join = Printf.sprintf "%s has joined the chat." name in
  broadcast_server join

(** Remove the client from the connected list and notify the room. *)
and disconnect addr =
  Connected.remove_addr addr >> fun (client, _) ->
  let name = Client.name client in
  Printf.sprintf "%s has left the chat." name |> broadcast_server

(** Send the client a help message. *)
and help name =
  Connected.find name >> fun (_, oc) ->
  Lwt_io.fprintf oc
    "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n"
    "-- ---------------------------------------------------------"
    (fmt_server "-- Welcome to lwt-chatroom! Commands are below.")
    "-- ---------------------------------------------------------"
    "-- [q]uit                 : Exit chatroom"
    "-- [h]elp                 : Display commands"
    "-- [l]ist                 : List connected clients"
    "-- [n]ick name            : Change name to [name]"
    "-- [c]olor color          : Change color to [color]"
    "-- [w]hisper name message : Send private [message] to [name]"
    "-- ---------------------------------------------------------"
    (Printf.sprintf "-- Where color is one of\n-- - %s\n-- - %s\n-- - %s\n-- - %s\n-- - %s\n-- - %s"
      (AT.sprintf [AT.Bold; AT.green] "green")
      (AT.sprintf [AT.Bold; AT.yellow] "yellow")
      (AT.sprintf [AT.Bold; AT.blue] "blue")
      (AT.sprintf [AT.Bold; AT.magenta] "magenta")
      (AT.sprintf [AT.Bold; AT.cyan] "cyan")
      (AT.sprintf [AT.Bold; AT.white] "white"))

(** Respond with the list of online clients. *)
and list_names name =
  Connected.find name >> fun (_, oc) ->
  let connected = Connected.to_client_list ()
  |> List.map Client.name
  |> List.fold_left (fun acc name -> acc ^ "-- " ^ name ^ "\n") ""
  in Lwt_io.fprintf oc
    "%s\n%s\n%s\n%s"
    ("-- ---------")
    (fmt_server "-- Connected")
    ("-- ---------")
    connected

(** Change the client's nickname and notify the room. *)
and change_name name name' =
  Connected.remove name >> fun (client, oc) ->
  if Connected.mem name' then begin
    Connected.insert client oc;
    Printf.sprintf "-- Error: %s is already taken." name'
    |> fmt_server
    |> Lwt_io.fprintl oc
  end else begin
    Connected.insert (Client.with_name client name') oc;
    Printf.sprintf "%s has changed their name to %s." client.name name'
    |> broadcast_server
  end

(** Change the client's color and notify the room. *)
and change_color name color =
  Connected.remove name >> fun (client, oc) ->
  let color' = parse_color client.color color in
  Connected.insert (Client.with_color client color') oc;
  Printf.sprintf
    "%s %s %s."
    (fmt_client_name client)
    (fmt_server "has changed their")
    (AT.sprintf [AT.Bold; color'] "color")
  |> broadcast_server

(** Send private message [m] to user [name']. *)
and whisper name name' m =
  Connected.find name >> fun (client, oc) ->
  Connected.find name' |> function
  | None -> Lwt_io.fprintl oc (fmt_server (Printf.sprintf "-- Error: %s is not online." name'))
  | Some (client', oc') ->
  let im = Printf.sprintf "[WHISPER FROM %s]: %s" (fmt_client_name client) m |> fmt_time in
  let om = Printf.sprintf "[WHISPER TO %s]: %s" (fmt_client_name client') m |> fmt_time in
  Lwt_io.fprintl oc om >>= fun () -> Lwt_io.fprintl oc' im

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
and (>>) opt f =
  match opt with 
  | None -> Lwt.fail_with "Disconnected"
  | Some client -> f client

(** Broadcast a message from the server. *)
and broadcast_server message =
  message |> fmt_server
          |> fmt_time
          |> broadcast

(** Broadcast a message from a client. *)
and broadcast_client addr message =
  match Connected.find addr with
  | None -> Lwt.return ()
  | Some (client, _) ->
    message |> fmt_client client
            |> fmt_time
            |> broadcast

(** Broadcast a message to all connected clients. *)
and broadcast message =
  print_endline message;
  () |> Connected.to_oc_list
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
let () = main ()
