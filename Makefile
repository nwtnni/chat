.PHONY = all

all:
	dune build

client:
	dune build && ./_build/default/bin/client.exe localhost 10000

server:
	dune build && ./_build/default/bin/server.exe
