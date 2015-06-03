{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , filepath, irc, mtl, network, old-locale, parsec, random
      , SafeSemaphore, stdenv, stm, time, unix
      }:
      mkDerivation {
        pname = "ircbot";
        version = "0.6.3";
        src = ./.;
        buildDepends = [
          base bytestring containers directory filepath irc mtl network
          old-locale parsec random SafeSemaphore stm time unix
        ];
        homepage = "http://hub.darcs.net/stepcut/ircbot";
        description = "A library for writing irc bots";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
