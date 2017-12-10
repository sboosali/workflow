{ mkDerivation, base, deepseq, doctest, hashable, hspec, QuickCheck
, semigroups, stdenv, X11
}:
mkDerivation {
  pname = "workflow-x11";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base deepseq hashable semigroups X11 ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  homepage = "http://github.com/sboosali/workflow-x11#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
