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

    flatpak.enable = true;

    autorandr = {
      enable = true;

      profiles = {
        "default" = {
          fingerprint = {
            eDP-1 = "*";
            HDMI-1 = "*";
          };
          config = {
            eDP-1 = {
              enable = true;
              primary = true;
              rate = "60.00";
              mode = "1920x1080";
              rotate = "normal";
              position = "0x0";
            };
            HDMI-1 = {
              enable = true;
              primary = false;
              mode = "1920x1080";
              rate = "74.97";
              rotate = "normal";
              position = "1920x0";
            };
          };
        };
      };
    };
  };

  nixpkgs.config = {
    allowUnfree = true;

    permittedInsecurePackages = [ "electron-13.6.9" ];
  };

}
