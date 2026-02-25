{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkMerge types;
  dotfilesConfig = ../../../dotfiles/config;
  configDirs = lib.attrNames (
    lib.filterAttrs (_: v: v == "directory") (builtins.readDir dotfilesConfig)
  );
  xdgConfigFromDotfiles = lib.genAttrs configDirs (name: {
    source = dotfilesConfig + "/${name}";
    recursive = true;
  });
in {
  options.hyprland = {
    enable = mkEnableOption {
      description = "Enables Hyprland";
      type = types.bool;
      default = false;
    };

    user = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "User to install default hyprland.conf for (via home-manager). Set to enable.";
    };
  };

  config = mkIf config.hyprland.enable (mkMerge [
    {
      programs.hyprland = {
        enable = true;
        withUWSM = true;
        xwayland.enable = true;
      };

      xdg.portal = {
        enable = true;
        extraPortals = with pkgs; [xdg-desktop-portal-hyprland];
      };

      # Login: greetd + regreet (ReGreet runs in Cage, lists Wayland sessions including Hyprland)
      services.greetd.enable = true;
      programs.regreet.enable = true;

      # Default Hyprland config (example) + packages referenced by it
      environment.systemPackages = with pkgs; [
        kdePackages.dolphin
        hyprlauncher
        hyprpaper
        hypridle
        brightnessctl
        playerctl
        waybar
      ];
    }
    
    (mkIf (config.hyprland.user != null) {
      home-manager.users.${config.hyprland.user} = {
        xdg.configFile = xdgConfigFromDotfiles // {
          "hypr/wallpaper.jpg" = {
            source = ../../themes/wallpaper5.jpg;
          };
        };
      };
    })
  ]);
}
