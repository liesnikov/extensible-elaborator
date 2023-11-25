{fetchFromGitHub, compiler ? ""}:
let
  unbound-src =
    fetchFromGitHub {
      owner = "liesnikov";
      repo = "unbound-generics";
      rev = "da6dddcb777939053b6106b95a9fe20c63b985fc";
      name = "unbound-generics-source";
      sha256 = "sha256-A34xzIbvwHyE5pYiMIszMsRI5rkYlrifM8P2nPAgziM=";
    };
  haskellOverride = haskellPackagesNew: haskellPackagesOld: rec {
    # because by default nixpkgs ships core-included mtl 2.2
    # this overrides it to the hackage version
    # will break with nixpkgs update though,
    # since the name "mtl_2_3_1" isn't stable
    # the reason not to do this:
    # a lot of binary caches get invalidated
    # mtl = haskellPackagesNew.mtl_2_3_1;
    unbound-generics = haskellPackagesNew.callCabal2nix "unbound-generics" unbound-src {};
  };
  overlay = final: prev:
    if compiler == ""
    then {
      haskellPackages = prev.haskellPackages.override {
          overrides = haskellOverride; };}
    else
      rec {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            "${compiler}" = prev.haskell.packages."${compiler}".override {
              overrides = haskellOverride; }; };};};
in overlay
