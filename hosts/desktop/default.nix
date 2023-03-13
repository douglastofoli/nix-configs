# Specific system configuration settings for desktop

{ lib, pkgs, user, location, host, ... }:

{
  imports = [ (import ./hardware-configuration.nix) ]
    ++ [ (import ../../modules/desktop/xmonad) ]
    ++ [ (import ../../modules/editors/emacs) ]
    ++ (import ../../modules/hardware)
    ++ [ (import ../../modules/programs/firefox.nix) ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
        extraEntries = {
          "windows.conf" = ''
            title Windows 11
            efi /EFI/Microsoft/Boot/bootmgfw.efi
            options root=PARTUUID=6CAE-7A1E
          '';
        };
      };
      efi.canTouchEfiVariables = true;
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
