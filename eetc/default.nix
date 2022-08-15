let
  defpkgs = builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-22.05.1271.babb041b716";
    url = "https://releases.nixos.org/nixos/22.05-small/nixos-22.05.1271.babb041b716/nixexprs.tar.xz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0g8vwni83zn6kgkczrm5vwmyhl473rrs9d4k4hn5gfbgfsyv7ls8";
  };
  config = compiler: {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              unbound-generics =
                let unbound-src =
                      pkgs.fetchzip {
                        url = "https://github.com/lambdageek/unbound-generics/archive/a2a558058012b09bb313b3eb03cddb734fcf4a98.zip";
                        sha256 = "af4592a93d0d280591b3bcff3ebe244956cc3637bf20ed2315fb6b2e070caef4";
  };
                in haskellPackagesNew.callCabal2nix "unbound-generics" unbound-src {};
            };
          };
        };
      };
    };
  };
in
{ compiler ? "ghc922",
  nixpkgs ? import defpkgs {config = (config compiler);} }:
nixpkgs.pkgs.haskell.packages."${compiler}".callCabal2nix "eetc" ./. {}
