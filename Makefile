all:
	dune build
	rm -f diffing
	ln -s _build/default/bin/main.exe diffing

clean:
	rm -f diffing
	rm -rf _build
	rm -f *~
	rm -f "#*"
	rm -f *.coverage

install:
	opam update
	opam install --yes . --deps-only
	eval $$(opam env)

build-cov:
	make clean
	BISECT_ENABLE=yes dune build

coverage: build-cov
	dune runtest
	bisect-ppx-report html
	bisect-ppx-report summary
