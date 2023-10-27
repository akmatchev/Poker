.PHONY: test

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

poker:
	OCAMLRUNPARAM=b dune exec lib/poker.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f ngrams.zip
	zip -r ngrams.zip . -x@exclude.lst

clean:
	dune clean
	rm -f ngrams.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh