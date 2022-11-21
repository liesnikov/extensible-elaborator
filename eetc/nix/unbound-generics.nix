{fetchzip, callCabal2nix}:
let unbound-src =
      fetchzip {
        url = "https://github.com/lambdageek/unbound-generics/archive/a2a558058012b09bb313b3eb03cddb734fcf4a98.zip";
        sha256 = "af4592a93d0d280591b3bcff3ebe244956cc3637bf20ed2315fb6b2e070caef4";
      };
in callCabal2nix "unbound-generics" unbound-src {}
