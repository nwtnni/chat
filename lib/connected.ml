module Connected = Map.Make (String)

let connected: (Client.t * Lwt_io.output Lwt_io.channel) Connected.t ref =
  ref Connected.empty

(** Get a string representation of a [Unix.sockaddr]. *)
let key_of_addr = function
| Unix.ADDR_UNIX (name) -> name
| Unix.ADDR_INET (addr, port) ->
  (Unix.string_of_inet_addr addr) ^ (string_of_int port)

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

(** Return the connected client with [addr], if they exist. *)
let find addr =
  let key = key_of_addr addr in
  Connected.find_opt key !connected

(** Return the connected client with [name], if they exist. *)
let find_name name =
  !connected |> Connected.bindings
             |> List.find_opt (fun (_, (client, _)) -> Client.name client = name)
             |> function None -> None | Some t -> Some (snd t)

(** Return all connected clients other than those with [addr]. *)
let sift addr =
  let key = key_of_addr addr in
  Connected.remove key !connected

(** Check if [name] is taken by a connected client. *)
let mem name =
  Connected.exists (fun _ (client, _) -> Client.name client = name) !connected

(** Mutably insert the client into the connected list. *)
let insert client oc =
  let open Client in
  let key = key_of_addr client.addr in
  connected := (Connected.add key (client, oc) !connected)

(** Mutably remove and return the client from the connected list. *)
let remove addr =
  let client = find addr in
  let others = sift addr in
  connected := others;
  client
