{
  description = "My Hakyll site generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=bb2009ca185d97813e75736c2b8d1d8bb81bde05";
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
          hakyll = (import ./vendor/hakyll).default;
          sitebuilder =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc944";
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
