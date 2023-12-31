let nixpkgs = import (fetchTarball("channel:nixos-23.11")) {};
in nixpkgs.mkShell {
  buildInputs = with nixpkgs; [zlib
                               haskell.compiler.ghc928
                               cabal-install
                               haskell-language-server
                               comrak
                               haskellPackages.hoogle
                               haskellPackages.fourmolu];
}
