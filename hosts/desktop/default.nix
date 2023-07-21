# Specific system configuration settings for desktop

{ config, lib, pkgs, user, location, host, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad) ]
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
        useOSProber = true;
      };

      timeout = 3;
    };
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ vaapiIntel vaapiVdpau libvdpau-va-gl ];
    driSupport = true;
    driSupport32Bit = true;
  };

  networking = {
    hostName = "nixos";
    networkmanager.enable = false;
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
  };

  environment.variables = { LIBVA_DRIVER_NAME = "i965"; };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 70;
  };

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };
  };

  services.blueman.enable = true;

  programs = {
    dconf.enable = true;
    git.config.user.signingkey = lib.mkForce host.gitSigningKey;
    nix-ld.enable = true;
  };

  nixpkgs.overlays = [
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://discord.com/api/download?platform=linux&format=tar.gz";
          sha256 = "0mr1az32rcfdnqh61jq7jil6ki1dpg7bdld88y2jjfl2bk14vq4s";
        };
      });
    })
  ];
}
