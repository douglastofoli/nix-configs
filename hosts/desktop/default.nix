# Specific system configuration settings for desktop

{ lib, pkgs, user, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../overlays) ] ++ [ (import ../../modules/desktop/xmonad) ]
    ++ (import ../../modules/hardware);

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
      };
      efi.canTouchEfiVariables = true;
      timeout = 1;
    };
  };

  services.blueman.enable = true;
}
