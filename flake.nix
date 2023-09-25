{
  description = "My Haskell.nix Project";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, haskellNix, nixpkgs, flake-utils, CHaP }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            P2P-PokerGame =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc945";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = { };
                  hlint = "3.6.1";
                  haskell-language-server = rec {
                    # HLS fix for stm-hamt bug
                    src = final.haskell-nix.sources."hls-1.10";
                    cabalProject = __readFile (src + "/cabal.project");
                  };
                };
                # Non-Haskell applications required by your project
                shell.buildInputs = with pkgs; [
                  bashInteractive
                  gnugrep
                  nixpkgs-fmt
                  nix-prefetch-git
                  nodejs-18_x
                  (vscode-with-extensions.override {
                    vscode = pkgs.vscodium;
                    vscodeExtensions = with pkgs.vscode-extensions; [
                      haskell.haskell
                      jnoortheen.nix-ide
                      justusadam.language-haskell
                      mkhl.direnv
                    ];
                  })
                ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.P2P-PokerGame.flake { };
      in
      flake // {
        # Built by `nix build .
        packages.default = flake.packages."P2P-PokerGame:exe:P2P-PokerGame";
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}