{
  description = "diasbruno site flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=24.11";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:

      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          name = "diasbruno_site";
          buildInputs = with pkgs; [zlib
                                    haskell.compiler.ghc98
                                    cabal-install
                                    haskell-language-server
                                    comrak
                                    haskellPackages.hoogle
                                    haskellPackages.fourmolu
                                    python312
                                    bun
                                   ];
        };
      });
}
