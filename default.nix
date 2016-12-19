{ mkDerivation, base, binary, directory, exceptions, filepath
, foldl, hakyll, lens, old-locale, pandoc, pandoc-types, parsec
, pipes, pipes-bytestring, pipes-group, pipes-safe, pipes-shell
, pipes-text, process, split, stdenv, strict, temporary, text, time
, transformers, aeson, yuicompressor
}:
mkDerivation {
  pname = "sitebuilder";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary directory exceptions filepath foldl hakyll lens
    old-locale pandoc pandoc-types parsec pipes pipes-bytestring
    pipes-group pipes-safe pipes-shell pipes-text process split strict
    temporary text time transformers aeson yuicompressor
  ];
  homepage = "http://github.com/jwiegley/sitebuilder";
  description = "A Hakyll site builder";
  license = stdenv.lib.licenses.unfree;
}
