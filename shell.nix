{ pkgs ? import <nixpkgs> {} }:
let cabal2nix = pkgs.cabal2nix;
in pkgs.mkShell {
  buildInputs = [ pkgs.cabal2nix ];
}
