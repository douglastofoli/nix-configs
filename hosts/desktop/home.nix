{ config, lib, pkgs, host, insync-v3, ... }:

{
  imports = [ (import ../../modules/desktop/hyprland/home.nix) ] ++ [ (import ../../modules/editors/nvim/home.nix) ];

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

      wezterm
    ];
  };

  programs.alacritty.settings.font.size = lib.mkForce host.alacrittyFontSize;

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = true;
  };
}
