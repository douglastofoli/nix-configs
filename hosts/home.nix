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
      pavucontrol

      # Apps
      firefox
      google-chrome
      obs-studio

      # File Management
      pcmanfm
      gnome.file-roller
      rsync
      unzip
      unrar
      zip
    ];

    file.".config/wallpaper.jpg".source = ../modules/themes/wallpaper2.jpg;

    pointerCursor = {
      gtk.enable = true;
      name = "Dracula-cursors";
      package = pkgs.dracula-theme;
      size = 16;
    };

    stateVersion = "22.11";
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
    font = { name = "JetBrains Mono Nerd Font"; };
  };
}
