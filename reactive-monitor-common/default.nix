{ mkDerivation, base, binary, hedgehog, hspec, hspec-hedgehog, lib
, text
}:
mkDerivation {
  pname = "reactive-monitor-common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary text ];
  testHaskellDepends = [ base hedgehog hspec hspec-hedgehog ];
  license = lib.licenses.bsd3;
}
