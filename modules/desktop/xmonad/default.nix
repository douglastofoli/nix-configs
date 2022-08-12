{ config, lib, pkgs, protocol, ... }:

{
  config = lib.mkIf (protocol == "X") {
    programs.dconf.enable = true;

    services = {
      xserver = {
        enable = true;

        layout = "br";
        xkbOptions = "eurosign:e";

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
          defaultSession = "none+xmonad";
        };

        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
            extraPackages = haskellPackages: [
              haskellPackages.hashable
              haskellPackages.xmobar
            ];
            # config = ./xmonad/xmonad.hs;
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
    ];
  };
}
