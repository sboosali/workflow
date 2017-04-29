{ mkDerivation, base, bytestring, doctest, exceptions, free, hspec
, http-types, QuickCheck, stdenv, transformers, workflow-types
}:
mkDerivation {
  pname = "workflow-extra";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring exceptions free http-types transformers
    workflow-types
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  homepage = "http://github.com/sboosali/workflow-extra#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
