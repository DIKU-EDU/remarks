{ nixpkgs ? import <nixpkgs> {} }:
(nixpkgs.pkgs.haskellPackages.callCabal2nix "remarks" ./. {})
