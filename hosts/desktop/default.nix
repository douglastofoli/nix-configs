# Specific system configuration settings for desktop

{ lib, pkgs, user, location, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/programs/steam.nix) ]
    ++ [ (import ../../modules/desktop/i3-gaps.nix) ]
    ++ [ (import ../../overlays) ];

  # ++ [ (import ../../modules/editors/emacs.nix) ]

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
