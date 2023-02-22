# Home manager configuration for desktop

{ config, pkgs, ... }:

{
  imports = [(import ../../modules/desktop/xmonad/home.nix)];

  home = {
    packages = with pkgs; [
      hugo
      jetbrains.datagrip
      gimp
      discord
      tdesktop
      obinskit
      obsidian
      qalculate-gtk
      qbittorrent
      youtube-music
      zoom-us
    ];
  };

  programs.alacritty.settings.font.size = 11;

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
