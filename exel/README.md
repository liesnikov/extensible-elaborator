exel -- **ex**tensible **el**aborator prototype
====

# To build the project #

```
nix-build . ""
```

the exel binary will be available in `result/bin/exel`

# To build module dependency graph #

```
nix-shell -p haskellPackages.graphmod graphviz
graphmod | dot -Tpdf > graph.pdf
xdg-open graph.pdf
```


# It should also be possible to build with cabal #
