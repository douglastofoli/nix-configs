{ lib, pkgs, protocol, ... }:

{
  programs.dconf.enable = true;

  services = {
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
                name = "Catppuccin-Mocha-Standard-Mauve-Dark";
                package = pkgs.catppuccin-gtk.override {
                  accents = [ "mauve" ];
                  size = "standard";
                  variant = "mocha";
                };
              };
              cursorTheme = {
                name = "Catppuccin-Mocha-Mauve-Cursors";
                package = pkgs.catppuccin-cursors.mochaMauve;
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
    feh
  ];
}
