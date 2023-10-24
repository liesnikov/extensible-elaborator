{
  description = "An implementation of extensible elaborator.";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
      in {
        packages = {
          inherit (pkgs.haskellPackages) eetc;
        };

        defaultPackage = self.packages.${system}.eetc;

        devShell = pkgs.haskellPackages.shellFor {
          packages = ps: with ps; [ eetc ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell-language-server
          ];
        };
      })) // {
        overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = finalhs: prevhs: let
              inherit (finalhs) callCabal2nixWithOptions;
              unbound-src = final.fetchFromGitHub {
                owner = "liesnikov";
                repo = "unbound-generics";
                rev = "da6dddcb777939053b6106b95a9fe20c63b985fc";
                name = "unbound-generics-source";
                sha256 = "sha256-A34xzIbvwHyE5pYiMIszMsRI5rkYlrifM8P2nPAgziM=";
              };
            in {
              eetc = callCabal2nixWithOptions "eetc" ./. "--no-check" {};
              unbound-generics = callCabal2nixWithOptions "unbound-generics" unbound-src "" {};
            };
          };
        };
      };
}
