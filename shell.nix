{ pkgs ? import <nixpkgs> {} }:

let
  override = pkgs.stdenv.lib.overrideDerivation;
in
  override (import ./shell.generated.nix) (old: {
    buildInputs = pkgs.haskellngPackages.ghcWithPackages (self:
      [ self.ghc-mod ] ++ old.buildInputs
    );
  })
