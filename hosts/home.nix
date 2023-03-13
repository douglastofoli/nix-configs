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
      obs-studio
      maim # screenshot

      # File Management
      # gnome.nautilus
      # gnome.file-roller
      rsync
      unzip
      unrar
      zip
    ];

    file.".config/wallpaper.jpg".source = ../modules/themes/wallpaper2.jpg;

    pointerCursor = {
      gtk.enable = true;
      name = "Catppuccin-Macchiato-Blue-Cursors";
      package = pkgs.catppuccin-cursors.macchiatoBlue;
      size = 24;
    };

    stateVersion = "22.05";
  };

  programs = { home-manager.enable = true; };

  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Macchiato-Standard-Blue-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "blue" ];
        size = "standard";
        variant = "macchiato";
      };
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override {
        accent = "blue";
        flavor = "macchiato";
      };
    };
    font = { name = "JetBrains Mono Nerd Font"; };
  };
}
