{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;
in pkgs.mkShell {
  buildInputs = with pkgs; [sbcl lispPackages.quicklisp comrak];
}
