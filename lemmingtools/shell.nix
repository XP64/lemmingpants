{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
with nixpkgs.haskell.lib;

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, megaparsec
      , postgresql-simple, stdenv, text
      }:
      mkDerivation {
        pname = "lemmingtools";
        version = "0.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring megaparsec postgresql-simple text
        ];
        executableHaskellDepends = [
          base bytestring megaparsec postgresql-simple text
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/githubuser/lemmingtools#readme";
        description = "Tools for helping out with Lemmingpants stuff";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskell.packages.ghc822
                       else pkgs.haskell.packages.${compiler};

  parser-combinators = haskellPackages.callPackage ./nix-support/parser-combinators-0.4.0.nix {};
  megaparsec = dontCheck (haskellPackages.callPackage ./nix-support/megaparsec-6.4.1.nix { parser-combinators = parser-combinators; });

  drv = haskellPackages.callPackage f { megaparsec = megaparsec; };

in

  if pkgs.lib.inNixShell then drv.env else drv
