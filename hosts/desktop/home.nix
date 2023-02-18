# Home manager configuration for desktop

{ pkgs, ... }:

{
  imports = [ (import ../../modules/desktop/hyprland/home.nix) ];

  home = {
    packages = with pkgs; [
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
      wakatime
      zoom-us
    ];
  };

  services = { blueman-applet.enable = true; };
}
