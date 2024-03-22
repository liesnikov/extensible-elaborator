{pkgs ? import (import ./nixpkgs.nix) {} }:
 pkgs.callPackage ./paper.nix {}
