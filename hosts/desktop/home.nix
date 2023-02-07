# Home manager configuration for desktop

{ pkgs, ... }:

{
  imports = [ (import ../../modules/desktop/hyprland/home.nix) ];

  home = {
    packages = with pkgs; [
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
      zoom-us
    ];
  };

  programs.alacritty.settings.font.size = 12;

  services = {
    blueman-applet.enable = true;
    picom.enable = true;
  };
}
