{ mkDerivation, base, containers, deepseq, enumerate, exceptions
, free, hashable, spiros, split, stdenv, template-haskell, text
, transformers
}:
mkDerivation {
  pname = "workflow-types";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers deepseq enumerate exceptions free hashable spiros
    split template-haskell text transformers
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://github.com/sboosali/workflow-types#readme";
  description = "Automate keyboard\/mouse\/clipboard\/application interaction";
  license = stdenv.lib.licenses.bsd3;
}
