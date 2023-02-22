{ lib, pkgs, protocol, ... }:

{
  programs.dconf.enable = true;

  services = {
    xserver = {
      enable = true;
      desktopManager.xterm.enable = false;
      
      layout = "br";

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
          enableConfiguredRecompile = true;
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
    haskellPackages.monad-logger
    haskellPackages.xmobar
    # haskellPackages.alsa-core
    # haskellPackages.alsa-mixer
    trayer
    xdotool
    ghc
    yad
    feh
  ];
}
