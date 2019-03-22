module Connected = Map.Make (String)

let connected: (Client.t * Lwt_io.output Lwt_io.channel) Connected.t ref =
  ref Connected.empty

(** Return the list of connected clients. *)
let to_client_list () =
  !connected |> Connected.bindings
             |> List.map snd
             |> List.map fst

(** Return the list of connected output channels. *)
let to_oc_list () =
  !connected |> Connected.bindings
             |> List.map snd
             |> List.map snd

(** Return the connected client with [name], if they exist. *)
let find name =
  Connected.find_opt name !connected

(** Return all connected clients other than those with [name]. *)
let sift name =
  Connected.remove name !connected

(** Check if [name] is taken by a connected client. *)
let mem name =
  Connected.exists (fun _ (client, _) -> Client.name client = name) !connected

(** Mutably insert the client into the connected list. *)
let insert client oc =
  let name = Client.name client in
  connected := (Connected.add name (client, oc) !connected)

(** Mutably remove and return the client from the connected list. *)
let remove name =
  let client = find name in
  let others = sift name in
  connected := others;
  client

let remove_addr addr =
  let client = !connected
    |> Connected.bindings
    |> List.map snd
    |> List.find_opt (fun (client, _) -> Client.addr client = addr)
  in
  connected := Connected.filter (fun _ (client, _) -> Client.addr client <> addr) !connected;
  client
