{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;
in pkgs.mkShell {
  buildInputs = with pkgs; [zlib
                            haskell.compiler.ghc928
                            cabal-install
                            haskell-language-server
                            comrak
                            haskellPackages.hoogle
                            haskellPackages.fourmolu];
}
