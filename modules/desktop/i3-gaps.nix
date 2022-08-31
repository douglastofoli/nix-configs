{ config, lib, pkgs, location, protocol, ... }:

{
  config = lib.mkIf (protocol == "X") {
    programs = {
      dconf.enable = true;
      nm-applet.enable = true;
    };

    services = {
      dbus = {
        enable = true;
        packages = [ pkgs.dconf ];
      };

      xserver = {
        enable = true;

        layout = "br";

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
            extraPackages = with pkgs; [ i3status ];
            configFile = ../../dotfiles/i3/config;
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
  };
}
