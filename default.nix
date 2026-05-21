{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./hdbc-postgresql.nix { inherit (pkgs) postgresql; }
