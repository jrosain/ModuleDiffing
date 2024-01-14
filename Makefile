all:
	dune build
	ln -s _build/main.native diffing

clean:
	rm -f diffing
	rm -rf _build
	rm -f *~
	rm -f "#*"
