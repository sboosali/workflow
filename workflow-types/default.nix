{ mkDerivation, base, comonad, containers, deepseq, Earley
, exceptions, free, hashable, semigroups, split, stdenv
, transformers
}:
mkDerivation {
  pname = "workflow-types";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base comonad containers deepseq Earley exceptions free hashable
    semigroups split transformers
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://github.com/sboosali/workflow-types#readme";
  description = "Automate keyboard\/mouse\/clipboard\/application interaction";
  license = stdenv.lib.licenses.bsd3;
}
