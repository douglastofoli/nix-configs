{ config, pkgs, nur, lib, ... }:

{
  hardware.new-lg4ff.enable = true;

  environment.systemPackages = [ config.nur.repos.c0deaddict.oversteer ];

  programs = {
    steam.enable = true;
    gamemode.enable = true;
  };

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "steam"
      "steam-original"
      "steam-runtime"
    ];
}
