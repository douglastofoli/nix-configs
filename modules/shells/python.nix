{pkgs, ...}: let
  packages = ps: with ps; [requests pip];
in {environment.systemPackages = [(pkgs.python3.withPackages packages)];}
