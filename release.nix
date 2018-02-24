{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.callPackage ./default.nix { inherit (pkgs.haskellPackages) mkDerivation hpack base vector-sized; }
