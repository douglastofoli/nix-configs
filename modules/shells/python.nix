{pkgs, ...}: let
  packages = ps: with ps; [httpx requests pip];
in {environment.systemPackages = [(pkgs.python3.withPackages packages)];}
