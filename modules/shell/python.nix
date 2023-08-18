{ pkgs, ... }:

let packages = ps: with ps; [ requests ];
in { environment.systemPackages = [ (pkgs.python3.withPackages packages) ]; }
