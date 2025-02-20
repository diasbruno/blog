{
  description = "";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=24.05";
  };

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [zlib
                                  haskell.compiler.ghc928
                                  cabal-install
                                  haskell-language-server
                                  comrak
                                  haskellPackages.hoogle
                                  haskellPackages.fourmolu];
      };
    };
}
