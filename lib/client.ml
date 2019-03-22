module AT = ANSITerminal

type t = {
  addr: Unix.sockaddr;
  color: AT.style;
  name: string;
}

(** Create a new client with the default color. *)
let make addr name =
  let color = AT.default in
  { addr; name; color }

(** Update [client] with new [name]. *)
let with_name client name =
  { client with name }

(** Update [client] with new [color]. *)
let with_color client color =
  { client with color }

let name client =
  client.name

let addr client =
  client.addr
