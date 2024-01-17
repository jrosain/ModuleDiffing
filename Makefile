.PHONY: all
all:
	dune build
	ln -f -s _build/default/bin/main.exe diffing
	chmod +x diffing

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: clean
clean:
	dune clean
	rm -f pieuvre

.PHONY: clean
tests: all
	dune runtest --force

