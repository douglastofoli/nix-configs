{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
in {
  options.xmonad = {
    enable = mkEnableOption {
      description = "Enables XMonad";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.xmonad.enable {
    services = {
      dbus = {
        enable = true;
        packages = with pkgs; [gnome-keyring];
      };

      displayManager.defaultSession = "none+xmonad";

      xserver = {
        enable = true;

        xkb = {
          layout = "br,us";
          options = "grp:alt_space_toggle";
        };

        displayManager.lightdm = {
          enable = true;
          greeters.gtk = {
            theme = {
              name = "Dracula";
              package = pkgs.dracula-theme;
            };
            cursorTheme = {
              name = "Dracula-cursors";
              package = pkgs.dracula-theme;
              size = 24;
            };
          };
        };

        windowManager.xmonad = {
          enable = true;
          enableContribAndExtras = true;
          config = ./xmonad.hs;
        };

        serverFlagsSection = ''
          Option "BlankTime" "0"
          Option "StandbyTime" "0"
          Option "SuspendTime" "0"
          Option "OffTime" "0"
        '';
      };
    };

    xdg.portal = {
      enable = true;
      config.common.default = "*";
      extraPortals = with pkgs; [xdg-desktop-portal-gtk];
    };

    environment.systemPackages = with pkgs; [
      haskellPackages.xmobar
      feh
      maim
      playerctl
      trayer
      xclip
      xorg.xwininfo
    ];
  };
}
