{pkgs ? import (import ../nixpkgs.nix) {} }:
 pkgs.callPackage ./abstract.nix {}
