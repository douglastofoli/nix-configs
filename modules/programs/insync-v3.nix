{ pkgs, ... }:

let
  nixpkgs_for_insync3 = with pkgs;
    callPackage (fetchFromGitHub {
      name = "nixpkgs";
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "b49473e6679c733f917254b786bfac42339875eb";
      sha256 = "1yan995h228v18b6hcjgvkbnaxwbbrink5if4ggvdans9mczcgfw";
    }) { };
in { home = { packages = with pkgs; [ nixpkgs_for_insync3.insync-v3 ]; }; }
