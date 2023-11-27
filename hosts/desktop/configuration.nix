# Specific system configuration settings for desktop
{
  lib,
  pkgs,
  ...
}: {
  imports =
    [./hardware-configuration.nix]
    ++ import ../../modules/desktops
    ++ import ../../modules/editors
    ++ import ../../modules/hardware
    ++ import ../../modules/programs
    ++ import ../../modules/services
    ++ import ../../modules/shells
    ++ import ../../modules/themes;

  docker.enable = true;
  xmonad.enable = true;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;

      grub = {
        enable = true;
        efiSupport = true;
        devices = ["nodev"];
        useOSProber = true;
      };

      timeout = 3;
    };
  };

  users.users.douglas = {
    isNormalUser = true;
    shell = pkgs.zsh;
    initialPassword = "123456";
    extraGroups = ["audio" "camera" "networkmanager" "video" "wheel"];
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [vaapiIntel];
    driSupport = true;
    driSupport32Bit = true;
  };

  networking = {
    hostName = "nixos";
    networkmanager.enable = false;
    nameservers = ["1.1.1.1" "1.0.0.1"];
  };

  environment = {
    variables = {LIBVA_DRIVER_NAME = "i965";};
    systemPackages = with pkgs; [
      nodejs_20

      gimp
      tdesktop
      spotify
      insomnia
      pcmanfm
      gnome.file-roller
      obinskit
      vscode
      dbeaver
      logseq
      obsidian
      insync

      cura
      # emacs/nvim
      fd
      ripgrep

      xplr

      (pkgs.discord.override {
        withOpenASAR = true;
        withVencord = true;
      })
    ];
  };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 60;
  };

  programs.nix-ld.enable = true;

  nixpkgs.overlays = [
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://discord.com/api/download?platform=linux&format=tar.gz";
          sha256 = "0qzdvyyialvpiwi9mppbqvf2rvz1ps25mmygqqck0z9i2q01c1zd";
        };
      });
      obinskit = super.obinskit.overrideAttrs (_: {
        src = builtins.fetchurl {
          url = "https://files.douglastofoli.dev/ObinsKit_1.2.11_x64.tar.gz";
          sha256 = "1kcn41wmwcx6q70spa9a1qh7wfrj1sk4v4i58lbnf9kc6vasw41a";
        };
      });
    })
  ];
}
