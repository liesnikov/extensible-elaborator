{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          unbound-generics = haskellPackagesNew.callPackage ./nix/unbound-generics.nix {};
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in pkgs.pkgs.haskellPackages.callPackage ./pi-forall.nix { }
