{ config, lib, pkgs, host, insync-v3, ... }:

{
  imports = [ (import ../../modules/editors/emacs/home.nix) ];

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

      inotify-tools

      insync-v3

      nodejs
    ];
  };

  programs.alacritty.settings.font.size = lib.mkForce host.alacrittyFontSize;

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
