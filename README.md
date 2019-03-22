# chat

Simple `Lwt`-based chat server.

Launch a local server and client with:

```bash
$ make server
$ make client
```

## Features

- Async-based concurrency
- Changeable nicknames
- Changeable colors
- Command help
- Connected list
- Message timestamps

## Example

```
-- Welcome to lwt-chatroom! Please enter a nickname.
Joe
[03/22/19 01:22:06] Joe has joined the chat.
-- --------------------------------------------
-- Welcome to lwt-chatroom! Commands are below.
-- --------------------------------------------
-- | [q]uit        : Exit chatroom
-- | [h]elp        : Display commands
-- | [l]ist        : List connected clients
-- | [n]ick name   : Change name to [name]
-- | [c]olor color : Change color to [color]
-- --------------------------------------------
-- Where color is one of
-- - green
-- - yellow
-- - blue
-- - magenta
-- - cyan
-- - white
[03/22/19 01:22:10] Billy has joined the chat.
[03/22/19 01:22:13] Billy: Yo
[03/22/19 01:22:15] Joe: Testing
[03/22/19 01:22:19] Billy has left the chat.
-- Connected
-- ---------
-- Joe
[03/22/19 01:22:26] Joe: :(
[03/22/19 01:22:47] Not Billy has joined the chat.
[03/22/19 01:22:49] Not Billy has changed their name to Billy.
[03/22/19 01:23:02] Billy: Hi
[03/22/19 01:23:04] Billy has left the chat.
```
