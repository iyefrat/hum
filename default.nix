{ mkDerivation, array, base, brick, bytestring, containers
, directory, filepath, lens, libmpd, mtl, regex-tdfa, relude
, stdenv, template-haskell, text, text-zipper, time, transformers
, vector, vty, witherable
}:
mkDerivation {
  pname = "hum";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base brick bytestring containers directory filepath lens
    libmpd mtl regex-tdfa relude template-haskell text text-zipper time
    transformers vector vty witherable
  ];
  executableHaskellDepends = [
    array base brick bytestring containers directory filepath lens
    libmpd mtl regex-tdfa relude template-haskell text text-zipper time
    transformers vector vty witherable
  ];
  testHaskellDepends = [
    array base brick bytestring containers directory filepath lens
    libmpd mtl regex-tdfa relude template-haskell text text-zipper time
    transformers vector vty witherable
  ];
  description = "A TUI MPD client, inspired by ncmpcpp";
  license = stdenv.lib.licenses.gpl2Plus;
}
