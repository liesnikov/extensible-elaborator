{ mkDerivation, array, base, containers, directory, filepath, HUnit
, lib, mtl, parsec, pretty, QuickCheck, transformers
, unbound-generics
}:
mkDerivation {
  pname = "pi-forall";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base containers directory filepath HUnit mtl parsec pretty
    transformers unbound-generics
  ];
  executableHaskellDepends = [
    array base containers directory filepath HUnit mtl parsec pretty
    transformers unbound-generics
  ];
  testHaskellDepends = [
    array base containers directory filepath HUnit mtl parsec pretty
    QuickCheck transformers unbound-generics
  ];
  homepage = "https://github.com/sweirich/pi-forall";
  description = "Demo implementation of typechecker for dependently-typed language";
  license = lib.licenses.bsd3;
}
