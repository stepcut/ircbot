{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , filepath, irc, mtl, network, old-locale, parsec, random
      , SafeSemaphore, stdenv, stm, time, unix, cabal-install
      }:
      mkDerivation {
        pname = "ircbot";
        version = "0.6.3";
        src = ./.;
        buildDepends = [
          base bytestring containers directory filepath irc mtl network
          old-locale parsec random SafeSemaphore stm time unix cabal-install
        ];
        homepage = "http://hub.darcs.net/stepcut/ircbot";
        description = "A library for writing irc bots";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
