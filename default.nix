{ stdenv, haskellngPackages }:

let
  env = haskellngPackages.ghcWithPackages (p: with p; [
    ansi-wl-pprint
    directory
    filepath
    free
    ini
    monad-control
    monad-par
    mtl
    optparse-applicative
    process
    text
    transformers
    transformers-base
    unordered-containers
    hspec
  ]);
in
  stdenv.mkDerivation {
    name = "gittins";
    buildInputs = [env];
    shellHook = ''
      export NIX_GHC="${env}/bin/ghc"
      export NIX_GHCPKG="${env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  }
