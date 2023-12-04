{
  custom-config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption types;
  cfg = custom-config.themes;
in {
  options.themes = {
    fontFamily = mkOption {
      description = "Set font family";
      type = types.str;
      default = "";
    };
    wallpaper = mkOption {
      description = "Set path of wallpaper";
      type = types.path;
      default = ./wallpaper.jpg;
    };
  };

  config = {
    home = {
      file.".config/wallpaper.jpg".source = cfg.wallpaper;

      pointerCursor = {
        gtk.enable = true;
        name = "Dracula-cursors";
        package = pkgs.dracula-theme;
        size = 24;
      };
    };

    gtk = {
      enable = true;
      theme = {
        name = "Dracula";
        package = pkgs.dracula-theme;
      };
      iconTheme = {
        name = "Dracula";
        package = pkgs.dracula-icon-theme;
      };
      font.name = cfg.fontFamily;
    };
  };
}
