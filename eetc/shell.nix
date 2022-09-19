{compiler ? "ghc922"}:
let
  pi-forall = import ./default.nix {inherit compiler;};
in
  pi-forall.env
