{ config, lib, pkgs, ... }:

let inherit (lib) mkEnableOption mkIf types;
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
      dbus.enable = true;

      xserver = {
        enable = true;

        layout = "br,us";
        xkbOptions = "grp:alt_space_toggle";

        displayManager = {
          lightdm = {
            enable = true;
            background =
              pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
            greeters = {
              gtk = {
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
          };
          defaultSession = "none+xmonad";
        };

        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
            config = ./xmonad.hs;
          };
        };

        serverFlagsSection = ''
          Option "BlankTime" "0"
          Option "StandbyTime" "0"
          Option "SuspendTime" "0"
          Option "OffTime" "0"
        '';
      };
    };

    xdg = {
      mime = {
        enable = true;
        defaultApplications = { "application/pdf" = "firefox.desktop"; };
      };

      portal = {
        enable = true;
        xdgOpenUsePortal = false;
        extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
      };
    };

    environment.systemPackages = with pkgs; [
      haskellPackages.xmobar

      feh
      maim

      trayer
      xdotool

      # xorg
      xclip
      xorg.xwininfo
    ];
  };
}
