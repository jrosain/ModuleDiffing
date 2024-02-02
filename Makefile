all:
	dune build
	rm -f diffing
	ln -s _build/default/bin/main.exe diffing

clean:
	rm -f diffing
	rm -rf _build
	rm -f *~
	rm -f "#*"

install:
	opam update
	opam install --yes . --deps-only
	eval $$(opam env)

coverage:
	make clean
	BISECT_ENABLE=yes dune build
	dune runtest
	bisect-ppx-report html
	bisect-ppx-report summary
