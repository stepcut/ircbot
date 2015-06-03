{ mkDerivation, base, bytestring, containers, directory, filepath
, irc, mtl, network, old-locale, parsec, random, SafeSemaphore
, stdenv, stm, time, unix
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
}
