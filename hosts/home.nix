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
      killall

      # Video/Audio
      pavucontrol
      vlc

      # Apps
      google-chrome
      obs-studio

      # File Management
      rsync
      unzip
      unrar
      wget
      zip
    ];

    file.".config/wallpaper.jpg".source = ../modules/themes/wallpaper5.jpg;

    pointerCursor = {
      gtk.enable = true;
      name = "Catppuccin-Mocha-Lavender-Cursors";
      package = pkgs.catppuccin-cursors.mochaLavender;
      size = 24;
    };

    stateVersion = "23.05";
  };

  programs = { home-manager.enable = true; };

  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Mocha-Standard-Lavender-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "lavender" ];
        variant = "mocha";
      };
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override {
        accent = "lavender";
        flavor = "mocha";
      };
    };
  };
}
