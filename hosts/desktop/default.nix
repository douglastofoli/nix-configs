# Specific system configuration settings for desktop

{ lib, pkgs, user, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/programs/steam.nix) ]
    ++ [ (import ../../modules/desktop/i3-gaps) ]
    ++ [ (import ../../overlays) ];

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

  services = {
    blueman.enable = true;

    xserver = {
      videoDrivers = [ "intel" ];

      resolutions = [{
        x = 2560;
        y = 1080;
      }];
    };
  };
}
