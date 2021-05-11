{ compiler ? "ghc8104"

, pkgs ? (import <darwin> {}).pkgs

# , rev    ? "28c2c0156da98dbe553490f904da92ed436df134"
# , sha256 ? "04f3qqjs5kd5pjmqxrngjrr72lly5azcr7njx71nv1942yq1vy2f"
# , pkgs   ? import (builtins.fetchTarball {
#     url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
#     inherit sha256; }) {
#     config.allowUnfree = true;
#   }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
, yuicompressor  ? pkgs.yuicompressor
}:

let haskell = pkgs.haskell.packages.${compiler}.override {
  overrides = self: super: with pkgs.haskell.lib; {
    pandoc          = dontCheck (doJailbreak (self.callHackage "pandoc" "2.11.4" {}));
    hakyll          = unmarkBroken (doJailbreak super.hakyll);
    time-compat     = doJailbreak super.time-compat;
    time-recurrence = unmarkBroken (doJailbreak super.time-recurrence);
  };
}; in

haskell.developPackage {
  root = ./.;
  inherit returnShellEnv;
}
