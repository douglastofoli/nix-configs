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
      qutebrowser
      nyxt
      obs-studio
      maim # screenshot

      # File Management
      gnome.nautilus
      gnome.file-roller
      rsync
      unzip
      unrar
      zip

      # Xorg
      xorg.xwininfo
    ];

    file.".config/wallpaper.jpg".source = ../modules/themes/wallpaper2.jpg;

    pointerCursor = {
      gtk.enable = true;
      name = "Catppuccin-Mocha-Mauve-Cursors";
      package = pkgs.catppuccin-cursors.mochaMauve;
      size = 24;
    };

    stateVersion = "22.11";
  };

  programs = { home-manager.enable = true; };

  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Mocha-Standard-Mauve-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "mauve" ];
        size = "standard";
        variant = "mocha";
      };
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override {
        accent = "mauve";
        flavor = "mocha";
      };
    };
    font = { name = "JetBrains Mono Nerd Font"; };
  };
}
