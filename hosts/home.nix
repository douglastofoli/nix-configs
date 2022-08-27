# General Home-manager configuration

{ config, lib, pkgs, user, ... }:

{
  imports = 
    (import ../modules/programs) ++
    (import ../modules/services) ++ 
    (import ../modules/shell);

  home = {
    username = "${user}";
    homeDirectory = "/home/${user}";

    packages = with pkgs; [
      asdf
      
      # Apps
      firefox
      google-chrome
      discord
      tdesktop
      signal-desktop

      # File Management
      rsync
      unzip
      unrar
      insync-v3

      # Video/Audio
      vlc
      spotify
      ffmpeg
      youtube-dl

      gnome.seahorse
      yad
      wakatime
    ];

    file.".config/wallpaper/".source = ../modules/themes/wallpaper;

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
