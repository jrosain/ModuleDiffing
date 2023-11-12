all:
	ocamlbuild -use-ocamlfind -pkg core -pkg ocamlgraph  main.native
	ln -f -s main.native diffing
	chmod +x diffing

clean:
	ocamlbuild -clean
	rm -f diffing
