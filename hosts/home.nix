# General Home-manager configuration

{ config, lib, pkgs, user, ... }:

{
  imports = (import ../modules/programs) ++ (import ../modules/services)
    ++ (import ../modules/shell);

  home = {
    username = "${user}";
    homeDirectory = "/home/${user}";

    packages = with pkgs; [
      discord
      ffmpeg
      firefox
      google-chrome
      html-tidy
      multimarkdown
      nodejs
      nodePackages.js-beautify
      nodePackages.npm
      nodePackages.stylelint
      rsync
      shellcheck
      shfmt
      signal-desktop
      spotify
      tdesktop
      unrar
      unzip
      vlc
      yad
      yarn
      youtube-dl
      wakatime
    ];

    file.".wallpaper.png".source = ../modules/themes/wallpaper.png;

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
