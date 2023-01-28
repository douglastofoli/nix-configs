# General Home-manager configuration

{ config, lib, pkgs, user, ... }:

{
  imports = (import ../modules/programs) ++ (import ../modules/services);

  home = {
    username = "${user}";
    homeDirectory = "/home/${user}";

    packages = with pkgs; [
      # Terminal
      btop

      # Video/Audio
      feh
      pavucontrol

      # Apps
      brave
      flameshot
      gnome.seahorse

      # File Management
      pcmanfm
      gnome.file-roller
      unzip
      unrar

      # Xorg
      picom
      xclip
      xorg.xev
      xorg.xkill
      xorg.xrandr
    ];

    file.".config/wallpaper.png".source = ../modules/themes/wallpaper.png;

    pointerCursor = {
      gtk.enable = true;
      name = "Dracula-cursors";
      package = pkgs.dracula-theme;
      size = 16;
    };

    stateVersion = "22.05";
  };

  programs = { home-manager.enable = true; };

  gtk = {
    enable = true;
    theme = {
      name = "Dracula";
      package = pkgs.dracula-theme;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };
}
