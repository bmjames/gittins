with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, abstract-par, ansi-wl-pprint, async, base
             , directory, filepath, free, hspec, ini, mtl, optparse-applicative
             , process, SafeSemaphore, stdenv, text, transformers
             , transformers-base, unordered-containers
             }:
             mkDerivation {
               pname = "gittins";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 abstract-par ansi-wl-pprint async base directory filepath free ini
                 mtl optparse-applicative process SafeSemaphore text transformers
                 transformers-base unordered-containers
               ];
               testDepends = [ base free hspec ];
               description = "Tool for managing multiple git repositories";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
