.SILENT:

all: build

build:
	dune build
	mv -f ./_build/default/main/main.exe ./DoomMetal

exec: build
	./DoomMetal 

clean:
	rm -f DoomMetal
	dune clean