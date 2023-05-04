{fetchzip, callCabal2nix}:
let unbound-src =
      fetchzip {
        url = "https://github.com/lambdageek/unbound-generics/archive/bd9bac1242ecb62d8152efc0e36357f2e1563fc5.zip";
        sha256 = "sha256-2naqNNVZ13XDPoCJD3JU8JNVOTi9WCFfTRAnSVyVxY0=";
      };
in callCabal2nix "unbound-generics" unbound-src {}
