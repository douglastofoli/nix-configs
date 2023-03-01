{ lib, pkgs, protocol, ... }:

{
  programs.dconf.enable = true;

  services = {
    xserver = {
      enable = true;
      desktopManager.xterm.enable = false;

      layout = "br,us";
      xkbModel = "pc105";
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

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };

  environment.systemPackages = with pkgs; [
    haskellPackages.dbus
    haskellPackages.List
    haskellPackages.monad-logger
    haskellPackages.xmobar
    haskellPackages.haskell-language-server

    trayer
    xdotool
    ghc
    yad
    feh
  ];
}
