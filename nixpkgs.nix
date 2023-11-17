builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-23.05.4461.21443a102b1a";
    # pull from https://channels.nix.gsc.io/
    url = "https://releases.nixos.org/nixos/23.05-small/nixos-23.05.4461.21443a102b1a/nixexprs.tar.xz";
    # Hash obtained using `nix-prefetch-url --type sha256 --unpack <url>`
    sha256 = "00f1s2vrkn9qa9h3vwsv2hrm0xq3vzvk70nxs2y7x0wayh7zpaqj";
}
