{pkgs, ...}: {
    kernel.python.plotting = {
    enable = true;
    extraPackages = ps: [ps.numpy ps.scipy ps.matplotlib ps.pandas];
  };
}
