compiler:
{
  packageOverrides = pkgs: rec {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${compiler}" = pkgs.haskell.packages."${compiler}".override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            unbound-generics =
              pkgs.callPackage ./unbound-generics.nix {callCabal2nix = haskellPackagesNew.callCabal2nix;};
          };
        };
      };
    };
  };
}
