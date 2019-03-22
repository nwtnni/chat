module AT = ANSITerminal

type t = {
  addr: Unix.sockaddr;
  color: AT.style;
  name: string;
}

let make addr name =
  let color = AT.default in
  { addr; name; color }

let with_name client name =
  { client with name }

let with_color client color =
  { client with color }

let name client =
  client.name

let to_string client =
  client.name
