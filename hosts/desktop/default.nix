# Specific system configuration settings for desktop

{ lib, pkgs, user, location, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad.nix) ]
    ++ [ (import ../../modules/editors/emacs.nix) ]
    ++ (import ../../modules/security);
  
  # ++ [ (import ../../modules/programs/steam.nix) ]
  # ++ [ (import ../../overlays) ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [ "btrfs" ];

    loader. = {
      efi.canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";

      grub = {
        enable = true;
        efiSupport = true;
        enableCryptodisk = true;
        devices = [ "nodev" ];
        device = "nodev";
        version = 2;
        extraEntries = ''
          menuentry "Windows 11" {
            insmod part_gpt
            insmod fat
            insmod search_fs_uuid
            insmod chain
            search --fs-uuid --set=root "0017-8B44"
            chainloader /EFI/Microsoft/Boot/bootmgfw.efi
          }
        '';
      };
    };
    
    initrd.luks.devices = {
      root = {
        device = "/dev/disk/by-uuid/4a4b273e-1008-405b-9e24-e3e24adbe5c9";
        preLVM = true;
      };
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

  time.hardwareClockInLocalTime = true;

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
