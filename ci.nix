{ pkgs ? import <nixpkgs> {} }:
let
  ghcs = [
  "ghc884"
  "ghc8107"
  "ghc928"
  "ghc947"
  "ghc963"
  ];
in
  pkgs.lib.genAttrs ghcs (x: pkgs.haskell.packages."${x}".callCabal2nix "ircbot" ./. {})
