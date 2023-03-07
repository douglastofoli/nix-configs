{ config, pkgs, ... }:

{
  imports = [ (import ../../modules/editors/emacs/home.nix) ];

  home = {
    packages = with pkgs; [
      google-drive-ocamlfuse
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

      inotify-tools

      ispell

      nodejs
    ];
  };

  programs.alacritty.settings.font.size = 11;

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
