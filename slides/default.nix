{pkgs ? import (import ../nixpkgs.nix) {} }:
 pkgs.callPackage ./slides.nix {}
