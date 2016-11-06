{ mkDerivation, base, binary
, directory, exceptions, filepath, foldl, hakyll, lens, old-locale
, pandoc, pandoc-types, parsec, pipes
, pipes-bytestring, pipes-group, pipes-safe, pipes-shell
, pipes-text, process, split, stdenv, strict, temporary, text, time
, transformers, yuicompressor
}:
mkDerivation {
  pname = "johnwiegley";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base binary directory exceptions
    filepath foldl hakyll lens old-locale pandoc pandoc-types parsec
    pipes pipes-bytestring pipes-group pipes-safe
    pipes-shell pipes-text process split strict temporary text time
    transformers yuicompressor
  ];
  homepage = "http://johnwiegley.com/";
  description = "What Thoughts May Come";
  license = stdenv.lib.licenses.unfree;
}
