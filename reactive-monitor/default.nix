{ mkDerivation, annotated-exception, async, base, binary, hedgehog
, hspec, hspec-hedgehog, lib, mtl, network, network-simple
, reactive-monitor-common, text, time
}:
mkDerivation {
  pname = "reactive-monitor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    annotated-exception async base binary mtl network network-simple
    reactive-monitor-common text time
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog hspec hspec-hedgehog ];
  license = lib.licenses.bsd3;
  mainProgram = "reactive-monitor";
}
