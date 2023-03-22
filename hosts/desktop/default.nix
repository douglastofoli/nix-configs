# Specific system configuration settings for desktop

{ lib, pkgs, user, location, host, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad) ]
    ++ [ (import ../../modules/editors/emacs) ]
    ++ (import ../../modules/hardware);

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;

      grub = {
        enable = true;
        efiSupport = true;
        devices = [ "nodev" ];
        version = 2;
        extraEntries = ''
          menuentry "Windows 11" {
            insmod part_gpt
            insmod fat
            insmod chain
            insmod search_fs_uuid
            search --fs-uuid --set=root "6CAE-7A1E"
            chainloader /EFI/Microsoft/Boot/bootmgfw.efi
          }
        '';
      };

      timeout = 3;
    };
  };

  environment.variables = { LIBVA_DRIVER_NAME = "i965"; };

  networking = {
    hostName = "wizarch";
    networkmanager.enable = true;
  };

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
    dbus.enable = true;
  };

  programs.git.config.user.signingkey = lib.mkForce host.gitSigningKey;

  nixpkgs.overlays = [
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://discord.com/api/download?platform=linux&format=tar.gz";
          sha256 = "12yrhlbigpy44rl3icir3jj2p5fqq2ywgbp5v3m1hxxmbawsm6wi";
        };
      });
    })
  ];
}
