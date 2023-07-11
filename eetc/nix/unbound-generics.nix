{fetchFromGitHub, callCabal2nix}:
let unbound-src =
      fetchFromGitHub {
        owner = "liesnikov";
        repo = "unbound-generics";
        rev = "04b33b774f6d27116232493135d58858a7ab35f4";
        name = "unbound-generics-source";
        sha256 = "sha256-sCe5hvKPkXG3DppHTc8UKTnAcntcRvnKZd62BZlQ6SQ=";
      };
in callCabal2nix "unbound-generics" unbound-src {}
