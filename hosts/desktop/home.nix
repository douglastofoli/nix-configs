{ config, lib, pkgs, ... }:

{
  imports = [ (import ../../modules/editors/nvim/home.nix) ];

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
      insync
      lazygit
      jdk17
      prismlauncher
      nodejs
      wezterm
    ];
  };

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
