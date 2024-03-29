{ nixpkgs ? import (import ../nixpkgs.nix) {} }:
let
  compiler = "ghc92";
  exel = import ./default.nix {inherit nixpkgs;};
  overlay = nixpkgs.callPackage ./nix/overlays.nix {inherit compiler;};
  pkgs = nixpkgs.extend overlay;
  haskell-language-server =
    if compiler == ""
    then pkgs.haskellPackages.haskell-language-server
    else pkgs.haskell.packages."${compiler}".haskell-language-server;
in
  exel.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++
                  [haskell-language-server];
  })
