{
  
  description = "An implementation of extensible elaborator.";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: (flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
  in {
    packages = {
      inherit (pkgs.haskellPackages) eetc;
    };

    defaultPackage = self.packages.${system}.Agda;

    devShell = pkgs.haskellPackages.shellFor {
      packages = ps: with ps; [ eetc ];
      nativeBuildInputs = with pkgs; [
        cabal-install
        haskell-language-server
        ormolu
      ];
    };
  })) // {
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override {
        overrides = self.haskellOverlay;
      };
    };

    haskellOverlay = final: prev: let
      inherit (final) callCabal2nixWithOptions;
    in {
      eetc = callCabal2nixWithOptions "eetc" ./. "--no-check" {}
    };
  };
}
