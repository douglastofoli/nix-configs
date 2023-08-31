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
      slack
      insomnia
      obsidian
      pcmanfm
      gnome.file-roller
      nodejs
      obinskit
      vscode

      nixfmt
      fractal
fluffychat
    ];
  };

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = false;
  };
}
