let nixpkgs = import (fetchTarball("channel:nixos-24.11")) {};
in nixpkgs.mkShell {
  buildInputs = with nixpkgs; [zlib
                               haskellPackages.ghc_9_10_1
                               cabal-install
                               haskellPackages.lsp
                               comrak
                               haskellPackages.hoogle
                               haskellPackages.fourmolu];
}
