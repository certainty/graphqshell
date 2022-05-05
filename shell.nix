 { pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    packages = [ 
      pkgs.haskell.compiler.ghc902
      pkgs.haskellPackages.haskell-language-server
    ];
  }