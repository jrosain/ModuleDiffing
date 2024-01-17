(lang dune 3.9)

(name ModuleDiffing)

(generate_opam_files true)


(authors "Camil Champin, Benoît Huftier, Corentin Moumard, Johann Rosain, Martin Trucchi")

(license LICENSE)

(package
 (name ModuleDiffing)
 (synopsis "Better error messages for OCaml modules through tree diffing algorithms")
 (description "This project aims towards giving better error messages for OCaml modules through tree diffing algorithms.")
 (depends ocaml dune))