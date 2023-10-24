let
  config = compiler: (import ./nix/compilerconfig.nix compiler);
in
{ compiler ? "ghc92",
  pkgs ? import (import ../nixpkgs.nix) {config = (config compiler);} }:
let
  pi-forall = import ./default.nix {inherit compiler; inherit pkgs;};
in
pi-forall.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++
                [pkgs.pkgs.haskell.packages."${compiler}".haskell-language-server
                ];
})
