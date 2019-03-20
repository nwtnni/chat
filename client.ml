open Lwt.Infix

(** Connect to the given IP address and port number. *)
let rec connect addr port =
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_INET (addr, port) in
  Lwt_unix.connect sock addr >|= fun () -> sock

(** Start the read-write connection to the server. *)
and loop sock =
  let ic = Lwt_io.of_fd Lwt_io.Input sock in
  let oc = Lwt_io.of_fd Lwt_io.Output sock in
  Lwt.choose [read ic; register oc]

(** Send a nickname to the server. *)
and register oc =
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | None -> Lwt.return ()
  | Some command ->
    Lwt_io.write_line oc command
    >>= fun () -> send oc

(** Receive and display server responses. *)
and read ic =
  Lwt_io.read_line_opt ic >>= function
  | None -> Lwt.return ()
  | Some message ->
    Lwt_io.printf "%s\n" message
    >>= fun () -> read ic

(** Send user commands to the server. *)
and send oc =
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | None -> Lwt.return ()
  | Some "q"
  | Some "quit" ->
    Lwt_io.write_line oc "quit"
    >>= fun () -> Lwt.return ()
  | Some command ->
    Lwt_io.print "\r\027[1A\027[K"
    >>= fun () -> Lwt_io.write_line oc command
    >>= fun () -> send oc

(** Parse either a domain name or an IP address. *)
let parse_addr name =
  try (Unix.gethostbyname name).h_addr_list.(0)
  with exn -> Unix.inet_addr_of_string name

(** Main entry point. *)
let main () =
  let name = parse_addr Sys.argv.(1) in
  let port = int_of_string Sys.argv.(2) in
  Lwt_main.run begin
    connect name port >>= loop
  end

(** Catch missing command-line arguments. *)
let () =
  try main () with exn -> print_endline "Usage: ./client.byte <SERVER> <PORT>"
