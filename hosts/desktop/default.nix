# Specific system configuration settings for desktop

{ lib, pkgs, user, location, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad.nix) ]
    ++ (import ../../modules/hardware);

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;

      grub = {
        enable = true;
        efiSupport = true;
        enableCryptodisk = true;
        devices = [ "nodev" ];
        version = 2;
        extraEntries = ''
          menuentry "Windows 11" {
            insmod part_gpt
            insmod fat
            insmod search_fs_uuid
            insmod chain
            search --fs-uuid --set=root "6CAE-7A1E"
            chainloader /EFI/Microsoft/Boot/bootmgfw.efi
          }
        '';
      };
    };
  };

  networking.hostName = "wizarch";

  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 60;
  };

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };
  };

  services = {
    blueman.enable = true;

    autorandr = {
      enable = true;

      profiles = {
        "default" = {
          fingerprint = { HDMI-2 = "*"; };
          config = {
            HDMI-2 = {
              enable = true;
              primary = true;
              rate = "74.99";
              mode = "2560x1080";
              rotate = "normal";
              position = "0x0";
            };
          };
        };
      };
    };
  };
}
