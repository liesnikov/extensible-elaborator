let
  defpkgs = builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-22.05.1271.babb041b716";
    url = "https://releases.nixos.org/nixos/22.05-small/nixos-22.05.1271.babb041b716/nixexprs.tar.xz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0g8vwni83zn6kgkczrm5vwmyhl473rrs9d4k4hn5gfbgfsyv7ls8";
  };
  config = compiler: (import ./nix/compilerconfig.nix compiler);
in
{ compiler ? "ghc922",
  nixpkgs ? import defpkgs {config = (config compiler);} }:
nixpkgs.pkgs.haskell.packages."${compiler}".callCabal2nix "eetc" ./. {}
