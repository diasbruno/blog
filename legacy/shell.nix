let nixpkgs = import (fetchTarball("channel:nixos-25.05")) {};
in nixpkgs.mkShell {
  buildInputs = with nixpkgs; [zlib
                               haskellPackages.ghc
                               cabal-install
                               haskellPackages.lsp
                               comrak
                               haskellPackages.hoogle
                               haskellPackages.fourmolu];
}
