# Specific system configuration settings for desktop

{ lib, pkgs, user, location, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad.nix) ]
    ++ [ (import ../../modules/editors/emacs.nix) ]
    ++ [ (import ../../modules/programs/steam.nix) ]
    ++ (import ../../modules/security) 
    ++ [ (import ../../overlays) ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    #supportedFilesystems = [ "btrfs" ];

    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
      };
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };
  };

  hardware.enableAllFirmware = true;

  networking = {
    hostName = "wizarch";

    useDHCP = lib.mkDefault false;
    interfaces.eno1.useDHCP = lib.mkDefault true;
    nameservers = [ "1.1.1.1" "1.0.0.1" ];

    networkmanager.enable = true;

    resolvconf.dnsExtensionMechanism = false;
  };

  environment.systemPackages = with pkgs; [ docker-compose ];

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };
  };

  services = {
    blueman.enable = true;

    xserver.resolutions = [{
      x = 2560;
      y = 1080;
    }];
  };
}
