{
  description = "My Hakyll site generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.sitebuilder.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          sitebuilder =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc961";
              shell.tools = {
                cabal = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkgconfig
              ];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."sitebuilder:exe:sitebuilder";
    });
}
