exel prototype
====

## To build the project ##

```
nix-build . ""
```

the exel binary will be available in `result/bin/exel`

nix-build by default will run `cabal test` and build hoogle entries for the project, so it can take a little while.

There's nothing specific to the nix build, except pinning version for reproducibility.
So in principle it should also be possible to build purely with cabal and I did it a couple of times with different compilers, from 9.2.8 which is used inside the nix build, to 9.8.1.
However, it wasn't thoroughly tested.

## To develop ##

Using [direnv](https://github.com/direnv/direnv) should allow you to both `cd` into the directory and have the toolchain avaialble, as well as loading the environment in [your editor](https://github.com/direnv/direnv/wiki#editor-integration).

## To build module dependency graph ##

```
nix-shell -p haskellPackages.graphmod graphviz
graphmod | dot -Tpdf > graph.pdf
xdg-open graph.pdf
```
