# General Home-manager configuration

{ config, lib, pkgs, user, ... }:

{
  imports = (import ../modules/programs) ++ (import ../modules/services)
    ++ [ (import ../modules/shell/zsh.nix) ];

  home = {
    username = "${user}";
    homeDirectory = "/home/${user}";

    packages = with pkgs; [
      # Terminal
      btop

      # Video/Audio
      feh
      pavucontrol
      vlc

      # Apps
      firefox
      google-chrome
      obs-studio
      maim # screenshot

      # File Management
      ranger
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

    stateVersion = "22.11";
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
    font = { name = "SauceCodePro Nerd Font Mono"; };
  };
}
