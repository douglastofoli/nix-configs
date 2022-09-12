{ lib, pkgs, user, location, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad.nix) ]
    ++ (import ../../modules/editors/emacs.nix)
    ++ [ (import ../../modules/services/gnome-keyring.nix) ]
    ++ (import ../../modules/hardware) ++ [ (import ../../overlays) ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [ "btrfs" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  hardware.enableAllFirmware = true;

  networking = {
    hostName = "solfacil";
    networkmanager.enable = true;
  };

  time.hardwareClockInLocalTime = true;

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };
  };

  services = {
    blueman.enable = true;

    xserver.resolutions = [
      {
        x = 1920;
        y = 1080;
      }
      {
        x = 1920;
        y = 1080;
      }
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;

    permittedInsecurePackages = [ "electron-13.6.9" ];
  };

}
