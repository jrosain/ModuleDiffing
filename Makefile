all:
	dune build
	rm -f diffing
	ln -s _build/default/bin/main.exe diffing

clean:
	rm -f diffing
	rm -rf _build
	rm -f *~
	rm -f "#*"
