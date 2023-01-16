.SILENT:
ARGS= --profile release 

all: build run

build:
	dune build $(ARGS)

run: build
	./_build/install/default/bin/DoomMetal
	
launch : exec

exec: clean
	dune exec DoomMetal $(ARGS)

clean:
	rm -rf _build