eetc
====

# To build the project #

```
nix-build . ""
```

the eetc binary will be available in `result/bin/eetc`

# To build module dependency graph #

```
nix-shell -p haskellPackages.graphmod graphviz
graphmod | dot -Tpdf graph.pdf
xdg-open graph.pdf
```
