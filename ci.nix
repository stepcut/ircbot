{ pkgs ? import <nixpkgs> {} }:
let
  ghcs = [
  "ghc844"
  "ghc865"
  "ghc883"
  "ghc8101"
  ];
in
  pkgs.lib.genAttrs ghcs (x: pkgs.haskell.packages."${x}".callCabal2nix "ircbot" ./. {})
