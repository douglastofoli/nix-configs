# Home manager configuration for desktop

{ config, pkgs, ... }:

{
  imports = [ (import ../../modules/desktop/hyprland/home.nix) ];

  home = {
    packages = with pkgs; [
      pulseaudio # installed to have pactl
      nodejs
      hugo
      jetbrains.datagrip
      etcher
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

  services = { blueman-applet.enable = true; };
}
