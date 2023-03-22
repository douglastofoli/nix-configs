{ lib, pkgs, protocol, ... }:

{
  programs.dconf.enable = true;

  services = {
    xserver = {
      enable = true;

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
                name = "Catppuccin-Macchiato-Standard-Blue-Dark";
                package = pkgs.catppuccin-gtk.override {
                  accents = [ "blue" ];
                  size = "standard";
                  variant = "macchiato";
                };
              };
              cursorTheme = {
                name = "Catppuccin-Macchiato-Blue-Cursors";
                package = pkgs.catppuccin-cursors.macchiatoBlue;
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

  xdg.portal.enable = true;

  environment.systemPackages = with pkgs; [
    haskellPackages.dbus
    haskellPackages.List
    haskellPackages.monad-logger
    haskellPackages.xmobar
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.cabal-install

    xdotool
    ghc
    yad
    trayer

    # xorg
    xclip
    xorg.xwininfo
  ];
}
