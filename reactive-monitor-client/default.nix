{ mkDerivation, base, binary, hedgehog, hspec, hspec-hedgehog, lib
, reactive-monitor-common, text
}:
mkDerivation {
  pname = "reactive-monitor-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary reactive-monitor-common text
  ];
  testHaskellDepends = [ base hedgehog hspec hspec-hedgehog ];
  license = lib.licenses.bsd3;
}
