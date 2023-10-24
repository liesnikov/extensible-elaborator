let
  config = compiler: (import ./nix/compilerconfig.nix compiler);
in
{ compiler ? "ghc92",
  pkgs ? import (import ../nixpkgs.nix) {config = (config compiler);} }:
pkgs.pkgs.haskell.packages."${compiler}".callCabal2nixWithOptions "eetc" ./. "--no-check" {}

# to disable tests run
# callCabal2nixWithOptions "eetc" ./. "--no-check" {}
