{ nixpkgs ? import (import ../nixpkgs.nix) {} }:
let
  compiler = "ghc92";
  overlay = nixpkgs.callPackage ./nix/overlays.nix {inherit compiler;};
  pkgs = nixpkgs.extend overlay;
  callCabal2nixWithOptions =
    if compiler == ""
    then pkgs.haskellPackages.callCabal2nixWithOptions
    else pkgs.haskell.packages."${compiler}".callCabal2nixWithOptions;
in
  callCabal2nixWithOptions "exel" ./. "" {}

# to disable tests run
# callCabal2nixWithOptions "exel" ./. "--no-check" {}
