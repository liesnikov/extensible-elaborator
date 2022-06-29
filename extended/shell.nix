{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  pi-forall = import ./default.nix {nixpkgs=nixpkgs; compiler=compiler;};

in

  pi-forall.env
