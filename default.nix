{ mkDerivation, async, base, lib, mtl, text, time }:
mkDerivation {
  pname = "reactive-monitor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ async base mtl text time ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "reactive-monitor";
}
