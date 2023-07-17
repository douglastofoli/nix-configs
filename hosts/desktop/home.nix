{ config, lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      hugo
      jetbrains.datagrip
      gimp
      discord
      tdesktop
      qbittorrent
      spotify
      insync
      lazygit
      slack

      insomnia

      pcmanfm
    ];
  };

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
