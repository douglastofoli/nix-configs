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
      qbittorrent
      spotify
      zoom-us
      insync
      lazygit
    ];
  };

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
