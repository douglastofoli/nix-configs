# General Home-manager configuration

{ config, lib, pkgs, user, ... }:

{
  imports = (import ../modules/programs) ++ (import ../modules/services)
    ++ (import ../modules/shell);

  home = {
    username = "${user}";
    homeDirectory = "/home/${user}";

    packages = with pkgs; [
      asdf

      # Apps
      firefox
      google-chrome
      # discord
      tdesktop
      signal-desktop

      # File Management
      rsync
      unzip
      unrar

      (import ../modules/programs/insync-v3.nix)

      # Video/Audio
      vlc
      spotify
      ffmpeg
      youtube-dl

      gnome.seahorse
      yad
      wakatime

      nodejs
      nodePackages.npm
      nodePackages.stylelint
      nodePackages.js-beautify
      yarn
      shfmt
      shellcheck
      html-tidy
      multimarkdown
    ];

    file.".wallpaper".source = ../modules/themes/wallpaper.png;

    pointerCursor = {
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
    font = { name = "Cantarell"; };
  };
}
