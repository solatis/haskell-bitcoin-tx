{ mkDerivation, base, binary, bitcoin-script, bitcoin-types
, bytestring, cryptohash, hexstring, hspec, lens, stdenv
}:
mkDerivation {
  pname = "bitcoin-tx";
  version = "0.13.1";
  src = ./.;
  buildDepends = [
    base binary bitcoin-script bitcoin-types bytestring cryptohash
    hexstring lens
  ];
  testDepends = [ base bitcoin-script bytestring hexstring hspec ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Utility functions for manipulating bitcoin transactions";
  license = stdenv.lib.licenses.mit;
}
