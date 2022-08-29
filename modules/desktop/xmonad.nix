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

        displayManager = {
          lightdm = {
            enable = true;
            background =
              pkgs.nixos-artwork.wallpapers.dracula.gnomeFilePath;
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
            enableConfiguredRecompile = true;
            extraPackages = haskellPackages: [
              haskellPackages.dbus
              haskellPackages.monad-logger
            ];
            #config = ../../dotfiles/xmonad/xmonad.hs;
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

    environment.systemPackages = with pkgs; [
      haskellPackages.haskell-language-server
      haskellPackages.hoogle
      haskellPackages.xmobar
      cabal-install
      stack
      trayer
      xdotool

      feh
      xorg.xwininfo
    ];
  };
}
