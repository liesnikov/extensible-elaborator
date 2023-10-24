{fetchFromGitHub, callCabal2nix}:
let unbound-src =
      fetchFromGitHub {
        owner = "liesnikov";
        repo = "unbound-generics";
        rev = "da6dddcb777939053b6106b95a9fe20c63b985fc";
        name = "unbound-generics-source";
        sha256 = "sha256-A34xzIbvwHyE5pYiMIszMsRI5rkYlrifM8P2nPAgziM=";
      };
in callCabal2nix "unbound-generics" unbound-src {}
