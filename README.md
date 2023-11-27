# ExEl #

This is a study on the design of **ex**tensible **el**aborators for dependently-typed languages.
The gist of the idea is to allow extensiblity in the constraints datatypes and solvers.

# Repository structure #

* `abstract` contains markdown sources for the TYPES abstract submission
* `exel` contains Haskell sources for the prototype implementation
* `paper` contains text and illustration files for the paper
* `slides` contains sources for the slides

# Building #

Everything in this repo is buildable with [nix](https://nixos.org), but can also be built with tools available.
For the LaTeX files you'll find dependencies listed in the `paper.nix`file (or `slides.nix`, `abstract.nix` respectively for each subfolder).
