{ mkDerivation, aeson, base, binary, directory, exceptions
, filepath, hakyll, old-locale, pandoc, pandoc-types, process
, split, stdenv, time, yaml, yuicompressor
}:
mkDerivation {
  pname = "sitebuilder";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary directory exceptions filepath hakyll old-locale
    pandoc pandoc-types process split time yaml yuicompressor
  ];
  homepage = "http://github.com/jwiegley/sitebuilder";
  description = "A Hakyll site builder";
  license = stdenv.lib.licenses.unfree;
}
