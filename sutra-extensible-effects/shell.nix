{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  packages = [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.ormolu
    haskellPackages.cabal2nix
    haskellPackages.haskell-dap
    haskellPackages.haskell-debug-adapter
    haskellPackages.ghci-dap
    haskellPackages.cabal-fmt
  ];
  shellHook = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias fmt="ormolu -i ./**/*.hs"
    alias repl="cabal repl"
    alias run="cabal run --"

  '';
}
