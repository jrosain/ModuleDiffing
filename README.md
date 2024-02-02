# ModuleDiffing

![Module Diffing Build & Test](https://github.com/jrosain/ModuleDiffing/actions/workflows/config.yml/badge.svg)
![Module Diffing Coverage](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/jrosain/3c06f9c5f9a2713932d8743db67e8b18/raw/ModuleDiffing.json)

This directory aims towards giving better error messages for OCaml modules through tree diffing algorithms.

## Dependencies

The dependencies can be installed by using `make install` or `opam install --yes . --deps-only`. 
If you want to install them manually, the following dependencies are needed.
* Library-wise:
  * `ocamlgraph` (which can be installed by `opam install ocamlgraph`).
  * `core_kernel` (which can be installed by `opam install core_kernel`).
* Test-wise:
  * `alcotest` (which can be installed by `opam install alcotest`).
  * `bisect_ppx` (which can be installed by `opam install bisect_ppx`).
