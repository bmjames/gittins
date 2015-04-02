with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, abstract-par, ansi-wl-pprint, base, directory
             , filepath, free, hspec, ini, monad-control, monad-par, mtl
             , optparse-applicative, process, stdenv, text, transformers
             , transformers-base, unordered-containers
             }:
             mkDerivation {
               pname = "gittins";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 abstract-par ansi-wl-pprint base directory filepath free ini
                 monad-control monad-par mtl optparse-applicative process text
                 transformers transformers-base unordered-containers
               ];
               testDepends = [ base free hspec ];
               description = "Tool for managing multiple git repositories";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
