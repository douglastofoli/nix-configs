# Specific system configuration settings for desktop

{ lib, pkgs, user, location, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ (import ../../modules/security) 
    ++ [ (import ../../modules/programs/steam.nix) ]
    ++ [ (import ../../modules/desktop/xmonad.nix) ]
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

  environment.systemPackages = with pkgs; [ docker-compose ];

  virtualisation.docker.enable = true;

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
