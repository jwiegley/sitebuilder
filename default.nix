{ compiler ? "ghc8107"

, rev    ? "a3a23d9599b0a82e333ad91db2cdc479313ce154"
, sha256 ? "05xmgrrnw6j39lh3d48kg064z510i0w5vvrm1s5cdwhdc2fkspjq"
, pkgs   ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null

, yuicompressor  ? pkgs.yuicompressor
}:

let haskell = pkgs.haskell.packages.${compiler}.override {
  overrides = self: super: with pkgs.haskell.lib; {
    # pandoc          = dontCheck (doJailbreak (self.callHackage "pandoc" "2.11.4" {}));
    hakyll          = unmarkBroken (doJailbreak super.hakyll);
    time-compat     = doJailbreak super.time-compat;
    time-recurrence = unmarkBroken (doJailbreak super.time-recurrence);
  };
}; in

haskell.developPackage {
  root = ./.;
  modifier = pkgs.haskell.lib.justStaticExecutables;
  inherit returnShellEnv;
}
