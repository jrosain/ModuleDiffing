all: 
	ocamlbuild -yaccflag -v -r -lib unix main.native
	ln -f -s main.native dico_diffing
	chmod +x dico_diffing

clean: 
	ocamlbuild -clean
	rm -f dico_diffing
