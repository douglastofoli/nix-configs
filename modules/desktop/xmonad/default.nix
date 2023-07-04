{ lib, pkgs, ... }:

{
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
                name = "Catppuccin-Mocha-Standard-Lavender-Dark";
                package = pkgs.catppuccin-gtk.override {
                  accents = [ "lavender" ];
                  variant = "mocha";
                };
              };
              cursorTheme = {
                name = "Catppuccin-Mocha-Lavender-Cursors";
                package = pkgs.catppuccin-cursors.mochaLavender;
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

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
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
}
