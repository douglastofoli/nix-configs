{pkgs, ...}: let
  packages = ps: with ps; [httpx requests pip ytmusicapi tkinter];
in {environment.systemPackages = [(pkgs.python3.withPackages packages)];}
