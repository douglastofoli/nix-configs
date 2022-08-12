{ config, lib, pkgs, protocol, ... }:

{
  config = lib.mkIf (protocol == "X") {
    programs = {
      dconf.enable = true;

      nm-applet.enable = true;
    };

    services = {
      xserver = {
        enable = true;

        layout = "br";
        xkbOptions = "eurosign:e";

        desktopManager.xterm.enable = false;

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
                  size = 16;
                };
              };
            };
          };
          defaultSession = "none+i3";
        };

        windowManager = {
          i3 = {
            enable = true;
            package = pkgs.i3-gaps;
            extraPackages = with pkgs; [ i3lock i3status i3blocks ];
            configFile = ./i3/config;
          };
        };

        serverFlagsSection = ''
          Option "BlankTime" "0"
          Option "StandbyTime" "0"
          Option "SuspendTime" "0"
          Option "OffTime" "0"
        '';

        resolutions = [
          {
            x = 1920;
            y = 1080;
          }
          {
            x = 2560;
            y = 1080;
          }
        ];
      };
    };

    environment.systemPackages = with pkgs; [
      xclip
      xorg.xev
      xorg.xkill
      xorg.xrandr

      feh
      gnome.file-roller
      pcmanfm
    ];
  };
}
