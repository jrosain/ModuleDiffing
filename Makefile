all:
	ocamlbuild -use-ocamlfind -pkg ocamlgraph -pkg core_kernel.pairing_heap main.native
	ln -f -s main.native diffing
	chmod +x diffing

clean:
	ocamlbuild -clean
	rm -f diffing
