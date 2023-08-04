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
      lazygit
      slack
      insomnia
      obsidian
      pcmanfm
      gnome.file-roller
      nodejs
    ];
  };

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = false;
  };
}
