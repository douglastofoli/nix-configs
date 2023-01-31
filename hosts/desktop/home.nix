# Home manager configuration for desktop

{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      jetbrains.datagrip
      etcher
      gimp
      discord
      tdesktop
      obinskit
      obsidian
      qalculate-gtk
      qbittorrent
    ];
  };

  programs.alacritty.settings.font.size = 12;

  services = {
    blueman-applet.enable = true;
    picom.enable = true;
  };
}
