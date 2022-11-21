compiler:
{
  packageOverrides = pkgs: rec {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${compiler}" = pkgs.haskell.packages."${compiler}".override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            unbound-generics =
              import ./unbound-generics.nix
                { fetchzip = pkgs.fetchzip;
                  callCabal2nix = haskellPackagesNew.callCabal2nix; };
          };
        };
      };
    };
  };
}
