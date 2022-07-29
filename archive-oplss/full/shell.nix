{compiler ? "ghc922"}:

let

  pi-forall = import ./default.nix {compiler=compiler;};

in

  pi-forall.env
