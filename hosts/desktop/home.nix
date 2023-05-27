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
      qbittorrent
      spotify
      zoom-us
      inotify-tools
      insync-v3
      lazygit

      jdk17
      prismlauncher

      nodejs
    ];
  };

  programs.alacritty.settings.font.size = lib.mkForce host.alacrittyFontSize;

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
