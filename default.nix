{ mkDerivation, base, deepseq, doctest, exceptions, free, hspec
, mtl, QuickCheck, semigroups, stdenv, transformers, workflow-types
}:
mkDerivation {
  pname = "workflow-pure";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base deepseq exceptions free mtl semigroups transformers
    workflow-types
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  homepage = "http://github.com/sboosali/workflow-pure#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
