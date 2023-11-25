# taken from haskell-nix-plugin
# which is broken as of nix 22.05, even for their own example.nix
# https://github.com/mpickering/haskell-nix-plugin/issues/1
# so the below doesn't work either
let
  defpkgs = builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-22.05.1271.babb041b716";
    url = "https://releases.nixos.org/nixos/22.05-small/nixos-22.05.1271.babb041b716/nixexprs.tar.xz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0g8vwni83zn6kgkczrm5vwmyhl473rrs9d4k4hn5gfbgfsyv7ls8";
  };
  plugin-overlay-git = builtins.fetchGit
   { url = https://github.com/mpickering/haskell-nix-plugin.git;}  ;
  plugin-overlay = import "${plugin-overlay-git}/overlay.nix";

  config = compiler: import ./nix/compilerconfig.nix compiler;

in
{ compiler ? "ghc922" }:
let
  nixpkgs = import defpkgs {compiler = (config compiler);
                            overlays = [plugin-overlay];};
  hl = nixpkgs.haskell.lib;
  hp = nixpkgs.haskellPackages;
  myPackage = import ./. {compiler = compiler; nixpkgs = nixpkgs;};
in
  (hp.withPlugin(plugs: ps: hl.addPlugin plugs.dump-core myPackage))
