# Specific system configuration settings for desktop

{ config, lib, pkgs, user, location, host, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad) ]
    ++ [ (import ../../modules/editors/emacs) ]
    ++ [ (import ../../modules/editors/nvim) ]
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
        useOSProber = true;
      };

      timeout = 3;
    };
  };

  networking = {
    hostName = "wizarch";
    networkmanager.enable = true;
  };

  xdg.mime.defaultApplications = {
    "application/pdf" = host.defaultBrowser;
    "text/html" = host.defaultBrowser;
    "x-scheme-handler/http" = host.defaultBrowser;
    "x-scheme-handler/https" = host.defaultBrowser;
    "x-scheme-handler/about" = host.defaultBrowser;
    "x-scheme-handler/unknown" = host.defaultBrowser;
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
