all:
	ocamlbuild main.native
	ln -f -s main.native string_diffing
	chmod +x string_diffing

clean:
	ocamlbuild -clean
	rm -f string_diffing
